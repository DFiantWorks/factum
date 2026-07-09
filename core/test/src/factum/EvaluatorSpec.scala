package factum

import factum.store.{DiskStore, MemoryStore}
import java.nio.file.{Files, Path}
import java.util.concurrent.atomic.AtomicInteger

class EvaluatorSpec extends munit.FunSuite:
  val dir = FunFixture[Path](
    setup = _ => Files.createTempDirectory("factum-eval"),
    teardown = deleteRecursively
  )

  def deleteRecursively(p: Path): Unit =
    if Files.isDirectory(p) then
      val stream = Files.list(p)
      try stream.forEach(deleteRecursively)
      finally stream.close()
    Files.deleteIfExists(p)

  test("second evaluation hits the cache"):
    val runs = AtomicInteger(0)
    val task = Task.pure("in").cached("step") { s =>
      runs.incrementAndGet()
      s.toUpperCase
    }
    val ev = Evaluator(MemoryStore())
    assertEquals(ev.eval(task), "IN")
    assertEquals(ev.eval(task), "IN")
    assertEquals(runs.get, 1)

  test("cache survives across evaluator instances via disk"):
    val root = Files.createTempDirectory("factum-eval")
    val runs = AtomicInteger(0)
    def task = Task.pure(7).cached("sq") { n => runs.incrementAndGet(); n * n }
    assertEquals(Evaluator(DiskStore(root)).eval(task), 49)
    assertEquals(Evaluator(DiskStore(root)).eval(task), 49)
    assertEquals(runs.get, 1)
    deleteRecursively(root)

  test("early cutoff: identical upstream output stops invalidation"):
    val store = MemoryStore()
    val upRuns = AtomicInteger(0)
    val downRuns = AtomicInteger(0)
    def pipeline(salt: Int) =
      Task.pure(salt)
        .cached("up") { _ => upRuns.incrementAndGet(); "same output" }
        .cached("down") { s => downRuns.incrementAndGet(); s.length }
    val ev = Evaluator(store)
    assertEquals(ev.eval(pipeline(1)), 11)
    assertEquals(ev.eval(pipeline(2)), 11) // up re-runs (new salt), same output
    assertEquals(upRuns.get, 2)
    assertEquals(downRuns.get, 1) // down never re-ran

  test("version bump, extraKey and cacheVersion each invalidate"):
    val store = MemoryStore()
    val runs = AtomicInteger(0)
    def task(version: Int, extra: Any) =
      Task.pure("x").cached("t", version, extra) { s => runs.incrementAndGet(); s }
    val ev = Evaluator(store)
    ev.eval(task(1, "a"))
    ev.eval(task(1, "a"))
    assertEquals(runs.get, 1)
    ev.eval(task(2, "a"))
    assertEquals(runs.get, 2)
    ev.eval(task(2, "b"))
    assertEquals(runs.get, 3)
    Evaluator(store, cacheVersion = "flushed").eval(task(2, "b"))
    assertEquals(runs.get, 4)

  test("zip fans in and diamond dependencies compute once per run"):
    val sharedRuns = AtomicInteger(0)
    val shared = Task.pure(1).cached("shared") { n => sharedRuns.incrementAndGet(); n + 1 }
    val left = shared.cached("left")(_ * 10)
    val right = shared.cached("right")(_ * 100)
    val top = left.zip(right).cached("top") { (l, r) => l + r }
    assertEquals(Evaluator(MemoryStore()).eval(top), 220)
    assertEquals(sharedRuns.get, 1)

  test("sequence evaluates all elements"):
    val tasks = Vector.tabulate(5)(i => Task.pure(i).cached(s"n$i")(_ * 2))
    assertEquals(Evaluator(MemoryStore()).eval(Task.sequence(tasks)), Vector(0, 2, 4, 6, 8))

  test("input is recomputed every run; unchanged value gives downstream hits"):
    var current = "v1"
    val inputRuns = AtomicInteger(0)
    val downRuns = AtomicInteger(0)
    val task = Task.input("cfg") { inputRuns.incrementAndGet(); current }
      .cached("process") { s => downRuns.incrementAndGet(); s.length }
    val ev = Evaluator(MemoryStore())
    assertEquals(ev.eval(task), 2)
    assertEquals(ev.eval(task), 2)
    assertEquals(inputRuns.get, 2) // always re-read
    assertEquals(downRuns.get, 1) // but downstream hit
    current = "v2-changed"
    assertEquals(ev.eval(task), 10)
    assertEquals(downRuns.get, 2)

  dir.test("source changes propagate, filtered noise does not"): root =>
    Files.writeString(root.resolve("top.vhd"), "entity a;")
    val runs = AtomicInteger(0)
    val task = Task.source(root, PathFilter("**.vhd"))
      .cached("compile") { fr => runs.incrementAndGet(); fr.digest.asString }
    val ev = Evaluator(MemoryStore())
    ev.eval(task)
    Files.writeString(root.resolve("sim.log"), "noise") // non-matching
    ev.eval(task)
    assertEquals(runs.get, 1)
    Files.writeString(root.resolve("top.vhd"), "entity b;") // matching
    ev.eval(task)
    assertEquals(runs.get, 2)

  dir.test("generated files restore after deletion, stale files are cleaned"): root =>
    val outDir = root.resolve("gen")
    val filter = PathFilter("**.vhd")
    val runs = AtomicInteger(0)
    val task = Task.pure("d").cachedWithFiles("commit") { s =>
      runs.incrementAndGet()
      Files.createDirectories(outDir)
      Files.writeString(outDir.resolve("top.vhd"), "entity;")
      (s, Vector(Output.Dir(outDir, filter)))
    }
    val store = DiskStore(root.resolve("cache"))
    Evaluator(store).eval(task)
    assertEquals(runs.get, 1)
    // wipe outputs + plant a stale matching file and an unmanaged file
    Files.delete(outDir.resolve("top.vhd"))
    Files.writeString(outDir.resolve("stale.vhd"), "old")
    Files.writeString(outDir.resolve("keep.log"), "unmanaged")
    Evaluator(store).eval(task)
    assertEquals(runs.get, 1) // restored, not recomputed
    assertEquals(Files.readString(outDir.resolve("top.vhd")), "entity;")
    assert(!Files.exists(outDir.resolve("stale.vhd")), "stale managed file must be cleaned")
    assert(Files.exists(outDir.resolve("keep.log")), "unmanaged file must be untouched")

  dir.test("corrupted restored file is re-materialized"): root =>
    val outDir = root.resolve("gen")
    val task = Task.pure("d").cachedWithFiles("commit") { s =>
      Files.createDirectories(outDir)
      Files.writeString(outDir.resolve("a.txt"), "correct")
      (s, Vector(Output.Dir(outDir)))
    }
    val store = DiskStore(root.resolve("cache"))
    Evaluator(store).eval(task)
    Files.writeString(outDir.resolve("a.txt"), "tampered")
    Evaluator(store).eval(task)
    assertEquals(Files.readString(outDir.resolve("a.txt")), "correct")

  test("evalUncached recomputes but warms the cache"):
    val store = MemoryStore()
    val runs = AtomicInteger(0)
    def task = Task.pure(1).cached("t") { n => runs.incrementAndGet(); n }
    val ev = Evaluator(store)
    ev.evalUncached(task)
    ev.evalUncached(task)
    assertEquals(runs.get, 2) // never reads
    ev.eval(task)
    assertEquals(runs.get, 2) // but the last uncached run warmed the cache

  test("cacheEnabled=false neither reads nor writes"):
    val store = MemoryStore()
    val runs = AtomicInteger(0)
    def task = Task.pure(1).cached("t") { n => runs.incrementAndGet(); n }
    Evaluator(store).eval(task) // seed the store
    val off = Evaluator(store, cacheEnabled = false)
    off.eval(task)
    off.eval(task)
    assertEquals(runs.get, 3)

  test("uncached steps run every evaluation and pass through"):
    val runs = AtomicInteger(0)
    val task = Task.pure(5).cached("c")(_ + 1).uncached("fire") { n =>
      runs.incrementAndGet()
      n * 10
    }
    val ev = Evaluator(MemoryStore())
    assertEquals(ev.eval(task), 60)
    assertEquals(ev.eval(task), 60)
    assertEquals(runs.get, 2)

  test("failures propagate and are not cached"):
    val runs = AtomicInteger(0)
    object Boom extends RuntimeException("boom")
    var fail = true
    def task = Task.pure(1).cached("t") { n =>
      runs.incrementAndGet()
      if fail then throw Boom
      n
    }
    val ev = Evaluator(MemoryStore())
    interceptMessage[RuntimeException]("boom")(ev.eval(task))
    fail = false
    assertEquals(ev.eval(task), 1) // failure was not cached
    assertEquals(runs.get, 2)

  test("task code runs with the evaluator creator's context classloader"):
    val marker = new java.net.URLClassLoader(Array.empty, getClass.getClassLoader)
    val original = Thread.currentThread().getContextClassLoader
    Thread.currentThread().setContextClassLoader(marker)
    try
      val ev = Evaluator(MemoryStore())
      val seen = ev.eval(
        Task.pure(()).uncached("capture")(_ => Thread.currentThread().getContextClassLoader)
      )
      assert(seen eq marker, s"worker context classloader was $seen, expected the marker loader")
    finally Thread.currentThread().setContextClassLoader(original)

  test("listener hooks fire on hit and compute"):
    val hits = AtomicInteger(0)
    val computes = AtomicInteger(0)
    val listener = new TaskListener:
      override def onCacheHit(name: String): Unit = hits.incrementAndGet()
      override def onComputed(name: String, durationMillis: Long): Unit =
        computes.incrementAndGet()
    val ev = Evaluator(MemoryStore(), listener = listener)
    def task = Task.pure(1).cached("t")(_ + 1)
    ev.eval(task)
    ev.eval(task)
    assertEquals(computes.get, 1)
    assertEquals(hits.get, 1)

  dir.test("concurrent evaluators sharing one disk store are safe"): root =>
    val store = DiskStore(root)
    val runs = AtomicInteger(0)
    def task = Task.pure("x").cached("t") { s =>
      runs.incrementAndGet()
      Thread.sleep(20)
      s + "!"
    }
    val results = java.util.concurrent.ConcurrentLinkedQueue[String]()
    val threads = (1 to 6).map(_ =>
      Thread(() =>
        results.add(Evaluator(store).eval(task)); ()
      )
    )
    threads.foreach(_.start())
    threads.foreach(_.join())
    assert(results.size == 6)
    results.forEach(r => assertEquals(r, "x!"))
    // separate runs may race to compute, but every result is correct and the
    // store converged to one cached entry
    assert(runs.get >= 1)
    assertEquals(Evaluator(store).eval(task), "x!")
end EvaluatorSpec
