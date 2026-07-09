package factum.ce

import cats.effect.IO
import factum.{Evaluator, Task}

/** Effect wrappers over the core evaluator. Evaluation is delegated to the core's own pool via
  * `IO.blocking`, keeping compute off the CE runtime while preserving referential transparency at
  * the call site.
  */
extension (evaluator: Evaluator)
  def evalIO[T](task: Task[T]): IO[T] = IO.blocking(evaluator.eval(task))
  def evalUncachedIO[T](task: Task[T]): IO[T] = IO.blocking(evaluator.evalUncached(task))
