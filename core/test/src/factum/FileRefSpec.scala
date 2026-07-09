package factum

import java.nio.file.{Files, Path}

class FileRefSpec extends munit.FunSuite:
  val dir = FunFixture[Path](
    setup = _ => Files.createTempDirectory("factum-fileref"),
    teardown = deleteRecursively
  )

  def deleteRecursively(p: Path): Unit =
    if Files.isDirectory(p) then
      val stream = Files.list(p)
      try stream.forEach(deleteRecursively)
      finally stream.close()
    Files.deleteIfExists(p)

  def write(root: Path, rel: String, content: String): Path =
    val p = root.resolve(rel)
    Files.createDirectories(p.getParent)
    Files.writeString(p, content)

  dir.test("content change changes digest, same content agrees"): root =>
    write(root, "a.txt", "one")
    val d1 = FileRef(root).digest
    assertEquals(FileRef(root).digest, d1)
    write(root, "a.txt", "two")
    assertNotEquals(FileRef(root).digest, d1)

  dir.test("file rename changes digest"): root =>
    write(root, "a.txt", "one")
    val d1 = FileRef(root).digest
    Files.move(root.resolve("a.txt"), root.resolve("b.txt"))
    assertNotEquals(FileRef(root).digest, d1)

  dir.test("non-matching files are invisible to a filtered ref"): root =>
    write(root, "rtl/top.vhd", "entity top;")
    val filter = PathFilter("**.vhd")
    val d1 = FileRef(root, filter).digest
    write(root, "sim.log", "noise")
    write(root, "work/junk.o", "junk")
    assertEquals(FileRef(root, filter).digest, d1)
    write(root, "rtl/top.vhd", "entity top2;")
    assertNotEquals(FileRef(root, filter).digest, d1)

  dir.test("changing the filter changes the digest"): root =>
    write(root, "a.vhd", "x")
    assertNotEquals(
      FileRef(root, PathFilter("**.vhd")).digest,
      FileRef(root, PathFilter("**.vhd", "**.sv")).digest
    )

  dir.test("missing path digests as valid absent state"): root =>
    val missing = root.resolve("does-not-exist")
    val fr = FileRef(missing)
    assertEquals(FileRef(missing).digest, fr.digest)
    write(root, "does-not-exist/now.txt", "here")
    assertNotEquals(FileRef(missing).digest, fr.digest)

  dir.test("single file refs work"): root =>
    val f = write(root, "one.txt", "1")
    val d1 = FileRef(f).digest
    Files.writeString(f, "2")
    assertNotEquals(FileRef(f).digest, d1)

  dir.test("quick mode detects size changes"): root =>
    val f = write(root, "one.txt", "aaaa")
    val d1 = FileRef(f, quick = true).digest
    Files.writeString(f, "aaaabbbb")
    assertNotEquals(FileRef(f, quick = true).digest, d1)

  dir.test("render/parse roundtrip preserves identity"): root =>
    write(root, "a.vhd", "x")
    val fr = FileRef(root, PathFilter(Seq("**.vhd"), Seq("work/**")))
    val parsed = FileRef.parse(FileRef.render(fr))
    assertEquals(parsed, fr)

  dir.test("revalidate=Always throws on stale parse"): root =>
    write(root, "a.txt", "x")
    val fr = FileRef(root, revalidate = FileRef.Revalidate.Always)
    val rendered = FileRef.render(fr)
    FileRef.parse(rendered) // fresh: fine
    write(root, "a.txt", "changed")
    intercept[FileRef.StaleFileRefException](FileRef.parse(rendered))

  dir.test("validate detects drift"): root =>
    write(root, "a.txt", "x")
    val fr = FileRef(root)
    assert(fr.validate)
    write(root, "a.txt", "y")
    assert(!fr.validate)
end FileRefSpec
