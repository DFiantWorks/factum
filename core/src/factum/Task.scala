package factum

import java.nio.file.Path

/** A declared task output on disk. */
enum Output derives CanEqual:
  /** A single generated file. */
  case File(path: Path)

  /** An entire folder, filtered, e.g. `Output.Dir(outDir, PathFilter("**.html"))`. The (dir,
    * filter) pair is the *managed scope*: only matching files are stored, restored, and cleaned.
    * Unmatched files in the same folder are invisible to Factum.
    */
  case Dir(path: Path, filter: PathFilter = PathFilter.all)

/** Evaluation hooks (cache-hit logging, timing, etc.). All methods default to no-ops. */
trait TaskListener:
  /** A cache hit for this node. Fires before any file restore; the cached value may never be
    * decoded at all (values materialize lazily, on demand).
    */
  def onCacheHit(name: String): Unit = ()
  def onComputed(name: String, durationMillis: Long): Unit = ()

  /** A cache hit is about to restore this node's declared output files/folders. Runs before
    * anything is materialized or cleaned, e.g. to tear down external state. `value` decodes the
    * cached value on demand (memoized); forcing it here is the only reason a hit of a non-demanded
    * upstream node would ever decode.
    */
  def onBeforeFilesRestore(name: String, value: () => Any): Unit = ()
  def onFilesRestored(name: String, count: Int): Unit = ()

  /** A cached entry was found for this node but its `validate` hook rejected it (see
    * [[Task.cached]]), so the node recomputes as if it had missed. Fires instead of `onCacheHit`,
    * never alongside it.
    */
  def onCacheInvalidated(name: String): Unit = ()
end TaskListener

object TaskListener:
  object noop extends TaskListener

/** A node in a static (applicative) task DAG. Tasks are pure descriptions: nothing runs until an
  * [[Evaluator]] evaluates them. Reuse task *values* to share nodes: a `val` referenced by two
  * downstream tasks is computed once per evaluation.
  */
sealed trait Task[T]:

  /** Fan-in: combine two independent tasks (evaluated in parallel). */
  def zip[U](that: Task[U]): Task[(T, U)] = Task.Zip(this, that)

  /** A cached computation node. `name` + `version` are the node's stable identity: bump `version`
    * whenever the logic of `f` changes. `extraKey` folds additional invalidation inputs (tool
    * versions, options, ...) into the cache key; it must be [[KeyHash]]-stable.
    *
    * `validate` is a hit-validation hook for values that record external state the cache key cannot
    * carry (e.g. files whose paths are only discovered while `f` runs, recorded inside the value):
    * on a cache hit it receives the decoded cached value and may reject it by returning false, in
    * which case the node recomputes as if it had missed, overwriting the entry under the same key
    * (downstream nodes re-key through the fresh output digest). A validator must return false
    * rather than throw; exceptions propagate. Supplying a validator forces the hit to decode the
    * value eagerly; the default ([[Task.alwaysValid]]) keeps the lazy on-demand decode.
    */
  def cached[U: Codec](
      name: String,
      version: Int = 1,
      extraKey: Any = (),
      validate: U => Boolean = Task.alwaysValid
  )(
      f: T => U
  ): Task[U] =
    Task.Cached[T, U](
      name,
      version,
      extraKey,
      this,
      t => (f(t), Vector.empty),
      summon[Codec[U]],
      validate
    )

  /** Like [[cached]], but `f` also declares generated files/folders. Declared outputs are stored in
    * the CAS and restored (with stale-file cleanup inside each managed scope) on cache hits.
    */
  def cachedWithFiles[U: Codec](
      name: String,
      version: Int = 1,
      extraKey: Any = (),
      validate: U => Boolean = Task.alwaysValid
  )(
      f: T => (U, Vector[Output])
  ): Task[U] =
    Task.Cached[T, U](name, version, extraKey, this, f, summon[Codec[U]], validate)

  /** Alias of [[cached]] for map-like readability. */
  def map[U: Codec](name: String, version: Int = 1)(f: T => U): Task[U] =
    cached(name, version)(f)

  /** Never cached; runs every evaluation. For downstream keying the step is assumed deterministic:
    * its output digest is derived from its input's digest. Use for live side effects (programming a
    * device, launching a viewer, etc.).
    */
  def uncached[U](name: String)(f: T => U): Task[U] = Task.Uncached(name, this, f)
end Task

object Task:
  /** The default `validate` of [[Task.cached]]: accepts every cached entry. The evaluator
    * recognizes this instance (by identity) and keeps the lazy on-demand value decode; any other
    * validator makes a hit decode eagerly so the value can be checked.
    */
  val alwaysValid: Any => Boolean = _ => true

  /** A constant. Participates in keying via its encoded content. */
  def pure[T: Codec](value: T): Task[T] = Pure(value, summon[Codec[T]])

  /** Recomputed every evaluation; its encoded *output* keys downstream tasks (an input whose value
    * didn't change causes downstream cache hits).
    */
  def input[T: Codec](name: String)(compute: => T): Task[T] =
    Input(name, () => compute, summon[Codec[T]])

  /** External file/folder dependency: digested (through `filter`, for folders) every evaluation;
    * downstream tasks key off the content digest. A missing path digests as a distinct, valid
    * "absent" state.
    */
  def source(path: Path, filter: PathFilter = PathFilter.all, quick: Boolean = false)
      : Task[FileRef] =
    Source(path, filter, quick)

  /** Multiple external file/folder dependencies, digested in parallel. */
  def sources(paths: Path*): Task[Vector[FileRef]] =
    Sequence(paths.toVector.map(source(_)))

  /** Evaluate many homogeneous tasks in parallel into one. */
  def sequence[T](tasks: Vector[Task[T]]): Task[Vector[T]] = Sequence(tasks)

  // --- node ADT (consumed by Evaluator) --------------------------------------------

  private[factum] final case class Pure[T](value: T, codec: Codec[T]) extends Task[T]
  private[factum] final case class Input[T](name: String, compute: () => T, codec: Codec[T])
      extends Task[T]
  private[factum] final case class Source(path: Path, filter: PathFilter, quick: Boolean)
      extends Task[FileRef]
  private[factum] final case class Zip[A, B](a: Task[A], b: Task[B]) extends Task[(A, B)]
  private[factum] final case class Sequence[T](tasks: Vector[Task[T]]) extends Task[Vector[T]]
  private[factum] final case class Cached[F, T](
      name: String,
      version: Int,
      extraKey: Any,
      prev: Task[F],
      run: F => (T, Vector[Output]),
      codec: Codec[T],
      validate: T => Boolean
  ) extends Task[T]
  private[factum] final case class Uncached[F, T](name: String, prev: Task[F], run: F => T)
      extends Task[T]
end Task
