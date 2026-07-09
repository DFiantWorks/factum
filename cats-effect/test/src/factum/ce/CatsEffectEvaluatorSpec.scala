package factum.ce

import cats.effect.IO
import factum.*
import factum.store.MemoryStore
import munit.CatsEffectSuite
import java.util.concurrent.atomic.AtomicInteger

class CatsEffectEvaluatorSpec extends CatsEffectSuite:
  test("evalIO caches like the core evaluator"):
    val runs = AtomicInteger(0)
    val task = Task.pure(3).cached("sq") { n => runs.incrementAndGet(); n * n }
    val ev = Evaluator(MemoryStore())
    for
      a <- ev.evalIO(task)
      b <- ev.evalIO(task)
    yield
      assertEquals(a, 9)
      assertEquals(b, 9)
      assertEquals(runs.get, 1)

  test("concurrent IO evaluations are safe"):
    val task = Task.pure("x").cached("t")(_ + "!")
    val ev = Evaluator(MemoryStore())
    import cats.syntax.parallel.*
    (1 to 8).toList.parTraverse(_ => ev.evalIO(task)).map { results =>
      results.foreach(r => assertEquals(r, "x!"))
    }
end CatsEffectEvaluatorSpec
