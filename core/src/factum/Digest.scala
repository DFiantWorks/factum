/*
 * Adapted for Factum from sbt (util-cache/src/main/scala/sbt/util/Digest.scala)
 * sbt
 * Copyright 2023, Scala center
 * Copyright 2011 - 2022, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * Licensed under Apache License 2.0 (see LICENSE)
 */
package factum

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.security.MessageDigest

/** A content digest in the form `algo-hexhash/sizeBytes`, e.g. `sha256-1f2a.../42`. The size suffix
  * makes the digest usable as a Bazel-REAPI-style CAS key.
  */
opaque type Digest = String

object Digest:
  val Sha256 = "sha256"

  extension (d: Digest)
    def asString: String = d
    def algo: String = d.substring(0, d.indexOf('-'))
    def hexHash: String = d.substring(d.indexOf('-') + 1, d.lastIndexOf('/'))
    def sizeBytes: Long = java.lang.Long.parseLong(d.substring(d.lastIndexOf('/') + 1))

  def apply(s: String): Digest =
    require(isValid(s), s"malformed digest string: $s")
    s

  def isValid(s: String): Boolean =
    val dash = s.indexOf('-')
    val slash = s.lastIndexOf('/')
    dash > 0 && slash > dash &&
    s.substring(dash + 1, slash).forall(c => c.isDigit || (c >= 'a' && c <= 'f')) &&
    s.substring(slash + 1).forall(_.isDigit) && s.length > slash + 1

  def apply(algo: String, digest: Array[Byte], sizeBytes: Long): Digest =
    s"$algo-${toHexString(digest)}/$sizeBytes"

  def sha256(bytes: Array[Byte]): Digest =
    val md = MessageDigest.getInstance("SHA-256")
    apply(Sha256, md.digest(bytes), bytes.length.toLong)

  def sha256(s: String): Digest = sha256(s.getBytes(StandardCharsets.UTF_8))

  def sha256File(path: Path): Digest =
    val md = MessageDigest.getInstance("SHA-256")
    var size = 0L
    val in = Files.newInputStream(path)
    try
      val buf = new Array[Byte](8192)
      var n = in.read(buf)
      while n >= 0 do
        md.update(buf, 0, n)
        size += n
        n = in.read(buf)
    finally in.close()
    apply(Sha256, md.digest(), size)

  private def toHexString(bytes: Array[Byte]): String =
    val sb = new StringBuilder(bytes.length * 2)
    for b <- bytes do sb.append(f"${b & 0xff}%02x")
    sb.toString

  /** Incremental digest builder. Every `update` overload is length-prefixed or type-tagged so that
    * concatenation ambiguities cannot produce colliding inputs.
    */
  final class Builder:
    private val md = MessageDigest.getInstance("SHA-256")
    private var size = 0L

    private def updateRaw(bytes: Array[Byte]): Unit =
      md.update(bytes)
      size += bytes.length

    def updateInt(value: Int): Builder =
      updateRaw(Array(
        (value >>> 24).toByte,
        (value >>> 16).toByte,
        (value >>> 8).toByte,
        value.toByte
      ))
      this

    def updateLong(value: Long): Builder =
      updateInt((value >>> 32).toInt)
      updateInt(value.toInt)

    def updateBytes(bytes: Array[Byte]): Builder =
      updateInt(bytes.length)
      updateRaw(bytes)
      this

    def updateString(s: String): Builder =
      updateBytes(s.getBytes(StandardCharsets.UTF_8))

    def updateDigest(d: Digest): Builder =
      updateString(d.asString)

    def result: Digest = apply(Sha256, md.digest(), size)
  end Builder
end Digest
