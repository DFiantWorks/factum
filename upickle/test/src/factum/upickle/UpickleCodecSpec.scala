package factum.upickle

import factum.*
import factum.store.MemoryStore
import _root_.upickle.default.ReadWriter
import java.util.concurrent.atomic.AtomicInteger

case class Design(name: String, width: Int, ports: List[String]) derives ReadWriter

class UpickleCodecSpec extends munit.FunSuite:
  test("derived codec roundtrips"):
    val d = Design("aes", 128, List("clk", "rst"))
    val c = summon[Codec[Design]]
    assertEquals(c.decode(c.encode(d)), d)

  test("derived codec drives cached tasks end to end"):
    val runs = AtomicInteger(0)
    val task = Task.pure("aes").cached("mkDesign") { name =>
      runs.incrementAndGet()
      Design(name, 128, List("clk"))
    }
    val ev = Evaluator(MemoryStore())
    assertEquals(ev.eval(task), Design("aes", 128, List("clk")))
    assertEquals(ev.eval(task), Design("aes", 128, List("clk")))
    assertEquals(runs.get, 1)
end UpickleCodecSpec
