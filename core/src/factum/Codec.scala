package factum

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, DataInputStream, DataOutputStream}
import java.nio.charset.StandardCharsets

/** Binary serialization typeclass for cached task values.
  *
  * Encoding should be deterministic: the digest of the encoded bytes is the task's output digest,
  * and early cutoff relies on equal values encoding to equal bytes.
  */
trait Codec[T]:
  def encode(value: T): Array[Byte]
  def decode(bytes: Array[Byte]): T

object Codec:
  def apply[T](using c: Codec[T]): Codec[T] = c

  /** Adapt existing string-based serialization (a pair of to/from-string functions) into a Codec.
    */
  def fromString[T](to: T => String, from: String => T): Codec[T] = new Codec[T]:
    def encode(value: T): Array[Byte] = to(value).getBytes(StandardCharsets.UTF_8)
    def decode(bytes: Array[Byte]): T = from(String(bytes, StandardCharsets.UTF_8))

  given string: Codec[String] = fromString(identity, identity)
  given unit: Codec[Unit] = fromString(_ => "", _ => ())
  given boolean: Codec[Boolean] = fromString(_.toString, _.toBoolean)
  given int: Codec[Int] = fromString(_.toString, _.toInt)
  given long: Codec[Long] = fromString(_.toString, _.toLong)
  given double: Codec[Double] = fromString(_.toString, _.toDouble)
  given byteArray: Codec[Array[Byte]] = new Codec[Array[Byte]]:
    def encode(value: Array[Byte]): Array[Byte] = value
    def decode(bytes: Array[Byte]): Array[Byte] = bytes

  given digest: Codec[Digest] = fromString(_.asString, Digest(_))
  given fileRef: Codec[FileRef] = fromString(FileRef.render, FileRef.parse)

  given option[T](using c: Codec[T]): Codec[Option[T]] = new Codec[Option[T]]:
    def encode(value: Option[T]): Array[Byte] = value match
      case None    => Array(0.toByte)
      case Some(v) => 1.toByte +: c.encode(v)
    def decode(bytes: Array[Byte]): Option[T] =
      if bytes(0) == 0 then None else Some(c.decode(bytes.drop(1)))

  given either[L, R](using cl: Codec[L], cr: Codec[R]): Codec[Either[L, R]] =
    new Codec[Either[L, R]]:
      def encode(value: Either[L, R]): Array[Byte] = value match
        case Left(l)  => 0.toByte +: cl.encode(l)
        case Right(r) => 1.toByte +: cr.encode(r)
      def decode(bytes: Array[Byte]): Either[L, R] =
        if bytes(0) == 0 then Left(cl.decode(bytes.drop(1)))
        else Right(cr.decode(bytes.drop(1)))

  given tuple2[A, B](using ca: Codec[A], cb: Codec[B]): Codec[(A, B)] = new Codec[(A, B)]:
    def encode(value: (A, B)): Array[Byte] =
      framed(Vector(ca.encode(value._1), cb.encode(value._2)))
    def decode(bytes: Array[Byte]): (A, B) =
      val Vector(a, b) = unframed(bytes): @unchecked
      (ca.decode(a), cb.decode(b))

  given tuple3[A, B, C](using ca: Codec[A], cb: Codec[B], cc: Codec[C]): Codec[(A, B, C)] =
    new Codec[(A, B, C)]:
      def encode(value: (A, B, C)): Array[Byte] =
        framed(Vector(ca.encode(value._1), cb.encode(value._2), cc.encode(value._3)))
      def decode(bytes: Array[Byte]): (A, B, C) =
        val Vector(a, b, c) = unframed(bytes): @unchecked
        (ca.decode(a), cb.decode(b), cc.decode(c))

  given vector[T](using c: Codec[T]): Codec[Vector[T]] = new Codec[Vector[T]]:
    def encode(value: Vector[T]): Array[Byte] = framed(value.map(c.encode))
    def decode(bytes: Array[Byte]): Vector[T] = unframed(bytes).map(c.decode)

  given list[T](using c: Codec[T]): Codec[List[T]] = new Codec[List[T]]:
    def encode(value: List[T]): Array[Byte] = framed(value.map(c.encode).toVector)
    def decode(bytes: Array[Byte]): List[T] = unframed(bytes).map(c.decode).toList

  /** Sorted by key on encode, so equal maps encode to equal bytes (early cutoff). */
  given stringMap[T](using c: Codec[T]): Codec[Map[String, T]] = new Codec[Map[String, T]]:
    def encode(value: Map[String, T]): Array[Byte] =
      framed(value.toVector.sortBy(_._1).flatMap { (k, v) =>
        Vector(k.getBytes(StandardCharsets.UTF_8), c.encode(v))
      })
    def decode(bytes: Array[Byte]): Map[String, T] =
      unframed(bytes).grouped(2).map { pair =>
        String(pair(0), StandardCharsets.UTF_8) -> c.decode(pair(1))
      }.toMap

  /** Length-prefixed concatenation of chunks. */
  def framed(chunks: Vector[Array[Byte]]): Array[Byte] =
    val bos = ByteArrayOutputStream()
    val out = DataOutputStream(bos)
    out.writeInt(chunks.length)
    chunks.foreach { chunk =>
      out.writeInt(chunk.length)
      out.write(chunk)
    }
    out.flush()
    bos.toByteArray

  def unframed(bytes: Array[Byte]): Vector[Array[Byte]] =
    val in = DataInputStream(ByteArrayInputStream(bytes))
    val count = in.readInt()
    Vector.fill(count) {
      val len = in.readInt()
      val chunk = new Array[Byte](len)
      in.readFully(chunk)
      chunk
    }
end Codec
