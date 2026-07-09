package factum
package store

import java.nio.charset.StandardCharsets
import java.nio.file.{
  AtomicMoveNotSupportedException,
  FileAlreadyExistsException,
  Files,
  Path,
  StandardCopyOption
}
import java.time.Instant
import java.util.UUID
import scala.jdk.CollectionConverters.*
import scala.util.Using

/** Disk-backed store, safe for concurrent use by multiple threads and processes.
  *
  * Layout under `root`:
  * {{{
  *   cas/sha256/<first-2-hex>/<rest-of-hex>_<size>   content-addressed blobs
  *   ac/<action-hex>.txt                              action results
  *   tmp/                                             staging for atomic writes
  * }}}
  *
  * Every write goes to `tmp/` first and is atomically moved into place. CAS names are pure
  * functions of content, so a lost race means the winner wrote the same bytes - either outcome is
  * correct, no locks needed.
  */
final class DiskStore(val root: Path) extends Store:
  private val casDir = root.resolve("cas")
  private val acDir = root.resolve("ac")
  private val tmpDir = root.resolve("tmp")
  Files.createDirectories(casDir)
  Files.createDirectories(acDir)
  Files.createDirectories(tmpDir)

  private def blobPath(digest: Digest): Path =
    val hex = digest.hexHash
    casDir.resolve(digest.algo).resolve(hex.take(2))
      .resolve(s"${hex.drop(2)}_${digest.sizeBytes}")

  private def actionPath(action: Digest): Path =
    acDir.resolve(s"${action.hexHash}_${action.sizeBytes}.txt")

  /** Atomically move `tmp` to `dest`. On filesystems without atomic move, or when losing a race to
    * a concurrent writer of the same content-addressed name, the existing destination is left in
    * place (idempotent writes make that correct).
    */
  private def moveIntoPlace(tmp: Path, dest: Path): Unit =
    Files.createDirectories(dest.getParent)
    try Files.move(tmp, dest, StandardCopyOption.ATOMIC_MOVE)
    catch
      case _: AtomicMoveNotSupportedException =>
        try Files.move(tmp, dest, StandardCopyOption.REPLACE_EXISTING)
        catch case _: FileAlreadyExistsException => Files.deleteIfExists(tmp)
      case _: FileAlreadyExistsException        => Files.deleteIfExists(tmp)
      case e: java.nio.file.FileSystemException =>
        // Windows: concurrent winner may briefly hold the destination open
        if Files.exists(dest) then Files.deleteIfExists(tmp) else throw e

  private def writeAtomically(dest: Path)(write: Path => Unit): Unit =
    val tmp = tmpDir.resolve(UUID.randomUUID().toString)
    try
      write(tmp)
      moveIntoPlace(tmp, dest)
    finally Files.deleteIfExists(tmp)

  def putBlob(bytes: Array[Byte]): Digest =
    val d = Digest.sha256(bytes)
    val dest = blobPath(d)
    if !Files.exists(dest) then writeAtomically(dest)(Files.write(_, bytes))
    d

  def putBlobFromFile(file: Path): Digest =
    val d = Digest.sha256File(file)
    val dest = blobPath(d)
    if !Files.exists(dest) then
      writeAtomically(dest)(Files.copy(file, _, StandardCopyOption.REPLACE_EXISTING))
    d

  def getBlob(digest: Digest): Option[Array[Byte]] =
    val p = blobPath(digest)
    if Files.exists(p) then Some(Files.readAllBytes(p)) else None

  def getBlobToFile(digest: Digest, dest: Path): Boolean =
    val p = blobPath(digest)
    Files.exists(p) && {
      if dest.getParent != null then Files.createDirectories(dest.getParent)
      Files.copy(p, dest, StandardCopyOption.REPLACE_EXISTING)
      true
    }

  def containsBlob(digest: Digest): Boolean = Files.exists(blobPath(digest))

  def getAction(action: Digest): Option[ActionResult] =
    val p = actionPath(action)
    if !Files.exists(p) then None
    else
      val result = ActionResult.parse(Files.readString(p, StandardCharsets.UTF_8))
      // touch for LRU-by-mtime GC; failure is harmless
      try Files.setLastModifiedTime(p, java.nio.file.attribute.FileTime.from(Instant.now))
      catch case _: java.io.IOException => ()
      Some(result)

  def putAction(action: Digest, result: ActionResult): Unit =
    writeAtomically(actionPath(action)) { tmp =>
      Files.writeString(tmp, ActionResult.render(result), StandardCharsets.UTF_8)
    }

  /** Delete action results older than `maxAgeMillis` (and beyond `maxTotalBytes` of blob budget,
    * oldest-action first), then delete CAS blobs unreachable from the surviving action results.
    * Safe mark-and-sweep: AC → CAS references are explicit.
    */
  def gc(maxAgeMillis: Long = Long.MaxValue, maxTotalBytes: Long = Long.MaxValue): Unit =
    val now = System.currentTimeMillis()
    val acFiles = Using.resource(Files.list(acDir))(_.iterator.asScala.toVector)
      .sortBy(p => -Files.getLastModifiedTime(p).toMillis)
    var budget = maxTotalBytes
    val kept = Vector.newBuilder[Path]
    for p <- acFiles do
      val age = now - Files.getLastModifiedTime(p).toMillis
      val result = ActionResult.parse(Files.readString(p, StandardCharsets.UTF_8))
      val resultSize = result.valueDigest.sizeBytes +
        result.outputFiles.map(_.digest.sizeBytes).sum +
        result.outputDirs.flatMap(_.entries).map(_.digest.sizeBytes).sum
      if age > maxAgeMillis || resultSize > budget then Files.deleteIfExists(p)
      else
        budget -= resultSize
        kept += p
    val reachable = kept.result().flatMap { p =>
      val r = ActionResult.parse(Files.readString(p, StandardCharsets.UTF_8))
      (r.valueDigest +:
        (r.outputFiles.map(_.digest) ++
          r.outputDirs.flatMap(_.entries).map(_.digest))).map(_.asString)
    }.toSet
    def sweep(dir: Path): Unit =
      if Files.isDirectory(dir) then
        Using.resource(Files.list(dir))(_.iterator.asScala.toVector).foreach(sweep)
      else
        val name = dir.getFileName.toString
        val parent = dir.getParent.getFileName.toString
        val algo = dir.getParent.getParent.getFileName.toString
        val (hexRest, size) = name.splitAt(name.lastIndexOf('_'))
        val digestStr = s"$algo-$parent$hexRest/${size.drop(1)}"
        if !reachable.contains(digestStr) then Files.deleteIfExists(dir)
    sweep(casDir)
  end gc
end DiskStore
