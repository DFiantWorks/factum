package factum

class KeyHashSpec extends munit.FunSuite:
  case class Options(name: String, level: Int, flags: List[String])

  test("equal values hash equally across instances"):
    val a = Options("fast", 3, List("x", "y"))
    val b = Options("fast", 3, List("x", "y"))
    assertEquals(KeyHash.digestOf(a), KeyHash.digestOf(b))
    assertEquals(KeyHash.digestOf((1, "a", true)), KeyHash.digestOf((1, "a", true)))

  test("different values hash differently"):
    assertNotEquals(
      KeyHash.digestOf(Options("fast", 3, Nil)),
      KeyHash.digestOf(Options("fast", 4, Nil))
    )
    assertNotEquals(KeyHash.digestOf("1"), KeyHash.digestOf(1))
    assertNotEquals(KeyHash.digestOf(Some(1)), KeyHash.digestOf(1))
    assertNotEquals(KeyHash.digestOf(List(1, 2)), KeyHash.digestOf(List(2, 1)))

  test("lambdas are rejected"):
    val f = (x: Int) => x + 1
    intercept[KeyHash.UnstableKeyException](KeyHash.digestOf(f))

  test("plain classes are rejected"):
    class NotStable
    intercept[KeyHash.UnstableKeyException](KeyHash.digestOf(NotStable()))

  test("unordered collections are rejected"):
    intercept[KeyHash.UnstableKeyException](KeyHash.digestOf(Set(1, 2, 3)))
    intercept[KeyHash.UnstableKeyException](KeyHash.digestOf(Map("a" -> 1)))

  test("nested case classes with options and eithers"):
    case class Outer(inner: Options, opt: Option[String], or: Either[Int, String])
    val v = Outer(Options("a", 1, List("z")), Some("s"), Right("r"))
    assertEquals(KeyHash.digestOf(v), KeyHash.digestOf(v.copy()))
end KeyHashSpec
