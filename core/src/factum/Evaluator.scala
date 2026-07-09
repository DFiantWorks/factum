package factum

import factum.store.{ActionResult, DirEntry, FileEntry, Store}
import java.nio.file.{Files, Path}
import java.util.concurrent.{
  CompletableFuture, CompletionException, ConcurrentHashMap,
  ForkJoinPool
}

/** Evaluates a [[Task]] DAG against a [[Store]].
  *
  * Independent branches run in parallel (up to `parallelism`); each node is evaluated at most once
  * per `eval` call, even when reached from multiple downstream tasks or from concurrent threads
  * (in-flight deduplication).
  *
  * @param cacheVersion
  *   global salt folded into every action digest; bump to flush
  * @param cacheEnabled
  *   when false, nothing is read from or written to the store
  */
final class Evaluator(
    store: Store,
    parallelism: Int = Runtime.getRuntime.availableProcessors(),
    cacheVersion: String = "1",
    cacheEnabled: Boolean = true,
    listener: TaskListener = TaskListener.noop
):
  // Worker threads must carry the context classloader of the Evaluator's creator:
  // JDK ForkJoinPool workers default to the system classloader, which breaks
  // classpath-resource lookups (e.g. scala.io.Source.fromResource) inside tasks
  // when running under layered classloaders (sbt run, servlet containers, ...).
  private val contextClassLoader = Thread.currentThread().getContextClassLoader
  private val pool = ForkJoinPool(
    parallelism,
    (p: ForkJoinPool) =>
      val t = ForkJoinPool.defaultForkJoinWorkerThreadFactory.newThread(p)
      t.setContextClassLoader(contextClassLoader)
      t
    ,
    null,
    false
  )

  /** Evaluate, using cached results where valid. */
  def eval[T](task: Task[T]): T =
    run(task, readCache = cacheEnabled, writeCache = cacheEnabled)

  /** Force recomputation of every cached node, but still write results through (a completed
    * uncached run warms the cache).
    */
  def evalUncached[T](task: Task[T]): T =
    run(task, readCache = false, writeCache = cacheEnabled)

  private def run[T](task: Task[T], readCache: Boolean, writeCache: Boolean): T =
    try Run(readCache, writeCache).evalNode(task).join().value
    catch case e: CompletionException => throw e.getCause

  /** Per-node result: the output digest is always materialized (downstream keying needs it), but
    * the *value* is a memoized thunk. On a fully-cached chain only the values actually demanded
    * (the final result, a computing node's input, a pre-restore hook that forces) are ever decoded.
    */
  private final class NodeResult[T](thunk: () => T, val outDigest: Digest):
    lazy val value: T = thunk()

  private final class Run(readCache: Boolean, writeCache: Boolean):
    private val memo = ConcurrentHashMap[Task[?], CompletableFuture[NodeResult[?]]]()

    def evalNode[T](task: Task[T]): CompletableFuture[NodeResult[T]] =
      // not computeIfAbsent: buildFuture recursively evaluates dependencies on this
      // same map, which computeIfAbsent forbids ("Recursive update"). Instead the
      // putIfAbsent winner builds the future and forwards its completion.
      val existing = memo.get(task)
      val future =
        if existing != null then existing
        else
          val fresh = CompletableFuture[NodeResult[?]]()
          val prev = memo.putIfAbsent(task, fresh)
          if prev != null then prev
          else
            try
              buildFuture(task).whenComplete { (r, e) =>
                if e != null then fresh.completeExceptionally(e) else fresh.complete(r)
              }
            catch case e: Throwable => fresh.completeExceptionally(e)
            fresh
      future.asInstanceOf[CompletableFuture[NodeResult[T]]]
    end evalNode

    /** Returns quickly: composes dependency futures; actual work runs on the pool. */
    private def buildFuture[T](task: Task[T]): CompletableFuture[NodeResult[?]] =
      val result: CompletableFuture[NodeResult[T]] = task match
        case Task.Pure(value, codec) =>
          CompletableFuture.supplyAsync(
            () => NodeResult(() => value, valueDigestOf(codec.encode(value))),
            pool
          )
        case Task.Input(_, compute, codec) =>
          CompletableFuture.supplyAsync(
            () =>
              val value = compute()
              NodeResult(() => value, valueDigestOf(codec.encode(value)))
            ,
            pool
          )
        case Task.Source(path, filter, quick) =>
          CompletableFuture.supplyAsync(
            () =>
              val fr = FileRef(path, filter, quick)
              NodeResult(() => fr, fr.digest)
            ,
            pool
          )
        case Task.Zip(a, b) =>
          evalNode(a).thenCombineAsync(
            evalNode(b),
            (ra, rb) =>
              val out = Digest.Builder().updateString("zip")
                .updateDigest(ra.outDigest).updateDigest(rb.outDigest).result
              NodeResult(() => (ra.value, rb.value), out)
            ,
            pool
          )
        case Task.Sequence(tasks) =>
          val futures = tasks.map(evalNode)
          CompletableFuture.allOf(futures*).thenApplyAsync(
            _ =>
              val results = futures.map(_.join())
              val b = Digest.Builder().updateString("seq").updateInt(results.length)
              results.foreach(r => b.updateDigest(r.outDigest))
              NodeResult(() => results.map(_.value), b.result)
            ,
            pool
          )
        case c: Task.Cached[?, T @unchecked] =>
          evalNode(c.prev).thenApplyAsync(dep => runCached(c, dep), pool)
        case u: Task.Uncached[?, T @unchecked] =>
          evalNode(u.prev).thenApplyAsync(
            dep =>
              val out = Digest.Builder().updateString("uncached").updateString(u.name)
                .updateDigest(dep.outDigest).result
              // uncached nodes run every evaluation, so the dep is genuinely needed
              val value = runPrev(u, dep.value)
              NodeResult(() => value, out)
            ,
            pool
          )
      result.asInstanceOf[CompletableFuture[NodeResult[?]]]
    end buildFuture

    private def runPrev[F, T](u: Task.Uncached[F, T], value: Any): T =
      u.run(value.asInstanceOf[F])

    private def runCached[F, T](c: Task.Cached[F, T], dep: NodeResult[?]): NodeResult[T] =
      val action =
        val b = Digest.Builder()
        b.updateString("cached")
        b.updateString(c.name)
        b.updateInt(c.version)
        b.updateString(cacheVersion)
        b.updateDigest(dep.outDigest)
        KeyHash.update(b, c.extraKey)
        b.result
      val fromCache = if readCache then store.getAction(action).flatMap(tryRestore(c, _)) else None
      fromCache.getOrElse(compute(c, action, dep))

    /** None when any required blob is missing (falls back to recomputation, which re-caches the
      * missing artifacts).
      */
    private def tryRestore[F, T](c: Task.Cached[F, T], res: ActionResult): Option[NodeResult[T]] =
      try
        if !store.containsBlob(res.valueDigest) then None
        else
          // decode lazily: on a fully-cached chain, a node's value is deserialized
          // only if something actually demands it
          val result = NodeResult[T](
            () =>
              c.codec.decode(store.getBlob(res.valueDigest).getOrElse(
                throw MissingBlobException(res.valueDigest, s"cached value of ${c.name}")
              )),
            outDigestOf(res)
          )
          listener.onCacheHit(c.name)
          if res.outputFiles.nonEmpty || res.outputDirs.nonEmpty then
            listener.onBeforeFilesRestore(c.name, () => result.value)
          var restored = 0
          for f <- res.outputFiles do
            if syncFile(Path.of(f.path), f.digest) then restored += 1
          for d <- res.outputDirs do restored += syncDir(d)
          if restored > 0 then listener.onFilesRestored(c.name, restored)
          Some(result)
      catch case _: MissingBlobException => None

    private def compute[F, T](
        c: Task.Cached[F, T],
        action: Digest,
        dep: NodeResult[?]
    ): NodeResult[T] =
      val t0 = System.nanoTime()
      val (value, outputs) = c.run(dep.value.asInstanceOf[F])
      val encoded = c.codec.encode(value)
      val valueDigest = if writeCache then store.putBlob(encoded) else Digest.sha256(encoded)
      val fileEntries = Vector.newBuilder[FileEntry]
      val dirEntries = Vector.newBuilder[DirEntry]
      for output <- outputs do
        output match
          case Output.File(path) =>
            val abs = path.toAbsolutePath.normalize
            val digest =
              if writeCache then store.putBlobFromFile(abs) else Digest.sha256File(abs)
            fileEntries += FileEntry(abs.toString.replace('\\', '/'), digest)
          case Output.Dir(path, filter) =>
            val abs = path.toAbsolutePath.normalize
            val entries = FileRef.walkFiltered(abs, filter).map { (rel, file) =>
              val digest =
                if writeCache then store.putBlobFromFile(file) else Digest.sha256File(file)
              FileEntry(rel, digest)
            }
            dirEntries += DirEntry(abs.toString.replace('\\', '/'), filter.canonical, entries)
      end for
      val result = ActionResult(valueDigest, fileEntries.result(), dirEntries.result())
      if writeCache then store.putAction(action, result)
      listener.onComputed(c.name, (System.nanoTime() - t0) / 1000000L)
      NodeResult(() => value, outDigestOf(result))
    end compute

    /** True if the file was materialized (false = was already up to date). */
    private def syncFile(dest: Path, digest: Digest): Boolean =
      val upToDate = Files.isRegularFile(dest) && Digest.sha256File(dest) == digest
      if !upToDate then
        if !store.getBlobToFile(digest, dest) then
          throw MissingBlobException(digest, dest.toString)
      !upToDate

    /** Sync a managed (dir, filter) scope to its manifest: materialize missing or stale matching
      * files, delete matching files not in the manifest, and never touch anything outside the
      * filter. Returns the number of changed files.
      */
    private def syncDir(d: DirEntry): Int =
      val dir = Path.of(d.dir)
      val filter = PathFilter.parseCanonical(d.filterCanonical)
      val manifest = d.entries.map(e => e.path -> e.digest).toMap
      var changed = 0
      if Files.isDirectory(dir) then
        for (rel, file) <- FileRef.walkFiltered(dir, filter) do
          manifest.get(rel) match
            case None =>
              Files.deleteIfExists(file) // stale leftover inside the managed scope
              changed += 1
            case Some(_) => () // freshness checked below via syncFile
      for (rel, digest) <- manifest do
        if syncFile(dir.resolve(rel), digest) then changed += 1
      changed
    end syncDir
  end Run

  private def valueDigestOf(encoded: Array[Byte]): Digest =
    Digest.Builder().updateString("value").updateBytes(encoded).result

  /** Output digest of a cached node: value plus all produced artifacts, so downstream keys change
    * when any declared output changes.
    */
  private def outDigestOf(res: ActionResult): Digest =
    val b = Digest.Builder().updateString("out").updateDigest(res.valueDigest)
    for f <- res.outputFiles do
      b.updateString(f.path)
      b.updateDigest(f.digest)
    for d <- res.outputDirs do
      b.updateString(d.dir)
      b.updateString(d.filterCanonical)
      b.updateInt(d.entries.length)
      for e <- d.entries do
        b.updateString(e.path)
        b.updateDigest(e.digest)
    b.result

  final class MissingBlobException(digest: Digest, dest: String)
      extends RuntimeException(
        s"cache blob ${digest.asString} for $dest is missing from the store"
      )
end Evaluator
