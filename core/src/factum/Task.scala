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
  def onCacheHit(name: String): Unit = ()
  def onComputed(name: String, durationMillis: Long): Unit = ()
  def onFilesRestored(name: String, count: Int): Unit = ()

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
    */
  def cached[U: Codec](name: String, version: Int = 1, extraKey: Any = ())(
      f: T => U
  ): Task[U] =
    Task.Cached[T, U](name, version, extraKey, this, t => (f(t), Vector.empty), summon[Codec[U]])

  /** Like [[cached]], but `f` also declares generated files/folders. Declared outputs are stored in
    * the CAS and restored (with stale-file cleanup inside each managed scope) on cache hits.
    */
  def cachedWithFiles[U: Codec](name: String, version: Int = 1, extraKey: Any = ())(
      f: T => (U, Vector[Output])
  ): Task[U] =
    Task.Cached[T, U](name, version, extraKey, this, f, summon[Codec[U]])

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
      codec: Codec[T]
  ) extends Task[T]
  private[factum] final case class Uncached[F, T](name: String, prev: Task[F], run: F => T)
      extends Task[T]
end Task
