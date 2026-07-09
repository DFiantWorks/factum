package factum

class DigestSpec extends munit.FunSuite:
  test("sha256 of known vector"):
    // sha256("abc") is a standard test vector
    val d = Digest.sha256("abc")
    assertEquals(
      d.asString,
      "sha256-ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad/3"
    )
    assertEquals(d.algo, "sha256")
    assertEquals(d.sizeBytes, 3L)
    assertEquals(d.hexHash.length, 64)

  test("parse validates format"):
    val ok = Digest.sha256("hello").asString
    assertEquals(Digest(ok).asString, ok)
    intercept[IllegalArgumentException](Digest("not-a-digest"))
    intercept[IllegalArgumentException](Digest("sha256-xyz/3"))
    intercept[IllegalArgumentException](Digest("sha256-abc123"))

  test("builder is deterministic and prefix-safe"):
    def build(f: Digest.Builder => Unit): Digest =
      val b = Digest.Builder()
      f(b)
      b.result
    assertEquals(build(_.updateString("ab")), build(_.updateString("ab")))
    // "a"+"b" must differ from "ab" (length prefixing)
    assertNotEquals(
      build { b => b.updateString("a"); b.updateString("b") },
      build(_.updateString("ab"))
    )
    assertNotEquals(build(_.updateInt(1)), build(_.updateInt(2)))
    assertNotEquals(build(_.updateLong(1L)), build(_.updateInt(1)))

  test("sha256File equals sha256 of content"):
    val tmp = java.nio.file.Files.createTempFile("factum", ".txt")
    java.nio.file.Files.writeString(tmp, "content")
    assertEquals(Digest.sha256File(tmp), Digest.sha256("content"))
    java.nio.file.Files.delete(tmp)
end DigestSpec
