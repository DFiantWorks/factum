package factum.upickle

import factum.Codec
// _root_: inside `package factum.upickle` a bare `upickle` resolves to this package
import _root_.upickle.default.{read, write, ReadWriter}
import java.nio.charset.StandardCharsets

/** Derive a Factum [[Codec]] from any upickle ReadWriter:
  * {{{
  * import factum.upickle.given
  * case class Design(name: String, size: Int) derives ReadWriter
  * // Codec[Design] now available for .cached(...) tasks
  * }}}
  */
given upickleCodec[T](using rw: ReadWriter[T]): Codec[T] with
  def encode(value: T): Array[Byte] = write(value).getBytes(StandardCharsets.UTF_8)
  def decode(bytes: Array[Byte]): T = read[T](String(bytes, StandardCharsets.UTF_8))
