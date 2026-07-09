package factum

/** Structural, stable hashing of cache-key material into a [[Digest.Builder]].
  *
  * Unlike `hashCode`/`MurmurHash3`, this is runtime-strict: values that cannot be hashed stably
  * across JVM runs (lambdas, plain classes with identity hashing) are rejected with an
  * [[UnstableKeyException]] instead of silently producing a key that never (or always) matches.
  *
  * Supported: `String`, primitives, `Unit`, `null`, `Option`, `Either`, `Digest`, [[FileRef]], Java
  * enums, Scala `Product`s (case classes/tuples/enums, hashed by class name, `productPrefix`, arity
  * and recursively by members), and ordered `Iterable`s / `Array`s of supported values. Unordered
  * collections (`Set`, `Map`) are rejected: their iteration order is not stable; sort them first.
  */
object KeyHash:
  final class UnstableKeyException(value: Any)
      extends IllegalArgumentException(
        s"unstable cache key of type ${value.getClass.getName}: $value\n" +
          "Cache keys must be stable across JVM runs. Use strings, primitives, case " +
          "classes, ordered collections, FileRef or Digest values - not lambdas, " +
          "plain classes, Sets or Maps."
      )

  def update(b: Digest.Builder, value: Any): Unit = value match
    case null        => b.updateString("null")
    case s: String   => b.updateString("s"); b.updateString(s)
    case i: Int      => b.updateString("i"); b.updateInt(i)
    case l: Long     => b.updateString("l"); b.updateLong(l)
    case bo: Boolean => b.updateString("b"); b.updateInt(if bo then 1 else 0)
    case d: Double   => b.updateString("d"); b.updateLong(java.lang.Double.doubleToLongBits(d))
    case f: Float    => b.updateString("f"); b.updateInt(java.lang.Float.floatToIntBits(f))
    case by: Byte    => b.updateString("y"); b.updateInt(by.toInt)
    case sh: Short   => b.updateString("h"); b.updateInt(sh.toInt)
    case c: Char     => b.updateString("c"); b.updateInt(c.toInt)
    case ()          => b.updateString("u")
    case fr: FileRef => b.updateString("fref"); b.updateDigest(fr.digest)
    case e: java.lang.Enum[?] =>
      b.updateString("e"); b.updateString(e.getDeclaringClass.getName); b.updateString(e.name)
    case _: Set[?] | _: Map[?, ?] => throw UnstableKeyException(value)
    case p: Product               =>
      b.updateString("p")
      b.updateString(p.getClass.getName)
      b.updateString(p.productPrefix)
      b.updateInt(p.productArity)
      p.productIterator.foreach(update(b, _))
    case it: Iterable[?] =>
      b.updateString("seq")
      b.updateInt(it.size)
      it.foreach(update(b, _))
    case arr: Array[?] =>
      b.updateString("arr")
      b.updateInt(arr.length)
      arr.foreach(update(b, _))
    case _ => throw UnstableKeyException(value)

  def digestOf(value: Any): Digest =
    val b = Digest.Builder()
    update(b, value)
    b.result
end KeyHash
