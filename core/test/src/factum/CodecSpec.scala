package factum

class CodecSpec extends munit.FunSuite:
  def roundtrip[T: Codec](value: T)(using munit.Location): Unit =
    assertEquals(Codec[T].decode(Codec[T].encode(value)), value)

  test("primitives roundtrip"):
    roundtrip("hello é— unicode") // non-ASCII must survive (UTF-8, not platform charset)
    roundtrip(42)
    roundtrip(42L)
    roundtrip(true)
    roundtrip(3.14)
    roundtrip(())

  test("containers roundtrip"):
    roundtrip(Option("x"))
    roundtrip(Option.empty[String])
    roundtrip[Either[Int, String]](Left(1))
    roundtrip[Either[Int, String]](Right("r"))
    roundtrip(("a", 1))
    roundtrip(("a", 1, true))
    roundtrip(Vector("a", "b", ""))
    roundtrip(List(1, 2, 3))
    roundtrip(Map("k1" -> 1, "k2" -> 2))

  test("map encoding is deterministic regardless of insertion order"):
    val c = Codec[Map[String, Int]]
    val a = c.encode(Map("a" -> 1, "b" -> 2, "c" -> 3))
    val b = c.encode(Map("c" -> 3, "a" -> 1, "b" -> 2))
    assert(a.sameElements(b))

  test("fromString adapts custom serialization"):
    case class Design(name: String, size: Int)
    given Codec[Design] = Codec.fromString(
      d => s"${d.name}:${d.size}",
      s =>
        val parts = s.split(':')
        Design(parts(0), parts(1).toInt)
    )
    roundtrip(Design("aes", 128))

  test("fileRef codec roundtrips"):
    val tmp = java.nio.file.Files.createTempDirectory("factum-codec")
    java.nio.file.Files.writeString(tmp.resolve("a.txt"), "a")
    val fr = FileRef(tmp, PathFilter("**.txt"))
    roundtrip(fr)
end CodecSpec
