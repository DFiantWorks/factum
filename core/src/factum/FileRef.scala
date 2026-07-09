/*
 * Adapted for Factum from Mill (core/api/java11/src/mill/api/PathRef.scala)
 * Copyright (c) Li Haoyi and Mill contributors
 * Licensed under the MIT License (see NOTICE)
 *
 * Reworked: SHA-256 digests instead of MD5-folded-to-Int, java.nio instead of
 * os-lib, filename filtering, no workspace aliasing, no POSIX permissions in the
 * digest (cross-OS digest parity is favored over permission tracking).
 */
package factum

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, LinkOption, Path}
import java.util.Base64
import scala.collection.mutable.ArrayBuffer
import scala.util.Using

/** A reference to a file or folder whose identity is the digest of its *content* (filtered, for
  * folders). Used both to key caches off external sources and to reference cached artifacts.
  */
final case class FileRef(
    path: Path,
    digest: Digest,
    quick: Boolean,
    filter: PathFilter,
    revalidate: FileRef.Revalidate
):
  def recompute: FileRef = FileRef(path, filter, quick, revalidate)
  def validate: Boolean = recompute.digest == digest
  override def toString: String = FileRef.render(this)

object FileRef:
  /** When a deserialized FileRef should be re-checked against the filesystem. */
  enum Revalidate derives CanEqual:
    case Never, Once, Always

  /** Digest a file or folder. A missing path digests as a distinct valid "absent" state (so sources
    * may legitimately not exist yet). For folders, only files matching `filter` participate; walk
    * order is normalized (sorted relative paths) and separators are `/`-normalized, so digests
    * agree across OSes.
    *
    * @param quick
    *   digest file attributes (mtime+size) instead of content
    */
  def apply(
      path: Path,
      filter: PathFilter = PathFilter.all,
      quick: Boolean = false,
      revalidate: Revalidate = Revalidate.Never
  ): FileRef =
    val b = Digest.Builder()
    b.updateString("FileRef")
    b.updateString(filter.canonical)
    val absPath = path.toAbsolutePath.normalize
    if !Files.exists(absPath, LinkOption.NOFOLLOW_LINKS) then b.updateString("absent")
    else if Files.isRegularFile(absPath, LinkOption.NOFOLLOW_LINKS) then
      b.updateString("file")
      digestEntry(b, absPath, quick)
    else
      b.updateString("dir")
      for (relPath, file) <- walkFiltered(absPath, filter) do
        b.updateString(relPath)
        digestEntry(b, file, quick)
    FileRef(absPath, b.result, quick, filter, revalidate)
  end apply

  private def digestEntry(b: Digest.Builder, file: Path, quick: Boolean): Unit =
    if Files.isSymbolicLink(file) then
      b.updateString("link")
      b.updateString(Files.readSymbolicLink(file).toString.replace('\\', '/'))
    else if quick then
      b.updateString("quick")
      b.updateLong(Files.getLastModifiedTime(file).toMillis)
      b.updateLong(Files.size(file))
    else
      b.updateString("content")
      // digest the file standalone and fold the digest in: stream-chunk boundaries
      // must not influence the result
      b.updateDigest(Digest.sha256File(file))

  /** All files under `root` matching `filter`, as sorted (relative `/`-path, path) pairs.
    * Directories fully covered by a double-star exclude anchored at a directory prefix are pruned
    * from the walk entirely.
    */
  private[factum] def walkFiltered(root: Path, filter: PathFilter): Vector[(String, Path)] =
    val results = ArrayBuffer.empty[(String, Path)]
    def rec(dir: Path): Unit =
      Using.resource(Files.newDirectoryStream(dir)) { stream =>
        stream.forEach { entry =>
          val rel = root.relativize(entry).toString.replace('\\', '/')
          if Files.isDirectory(entry, LinkOption.NOFOLLOW_LINKS) then
            if !filter.prunesDir(rel) then rec(entry)
          else if filter.matches(rel) then results += ((rel, entry))
        }
      }
    rec(root)
    results.sortBy(_._1).toVector

  // --- wire format: "fref|<rev>|<q|c>|<digest>|<filterB64>|<path>" -----------------

  private val b64e = Base64.getUrlEncoder.withoutPadding
  private val b64d = Base64.getUrlDecoder

  def render(fr: FileRef): String =
    val rev = fr.revalidate match
      case Revalidate.Never  => "v0"
      case Revalidate.Once   => "v1"
      case Revalidate.Always => "vn"
    val q = if fr.quick then "q" else "c"
    val filterB64 =
      b64e.encodeToString(fr.filter.canonical.getBytes(StandardCharsets.UTF_8))
    s"fref|$rev|$q|${fr.digest.asString}|$filterB64|${fr.path.toString.replace('\\', '/')}"

  def parse(s: String): FileRef =
    val parts = s.split('|')
    require(parts.length >= 6 && parts(0) == "fref", s"malformed FileRef string: $s")
    val revalidate = parts(1) match
      case "v0"  => Revalidate.Never
      case "v1"  => Revalidate.Once
      case "vn"  => Revalidate.Always
      case other => throw IllegalArgumentException(s"bad revalidate token: $other")
    val quick = parts(2) == "q"
    val digest = Digest(parts(3))
    val filterCanonical = String(b64d.decode(parts(4)), StandardCharsets.UTF_8)
    // path may itself contain '|' on some filesystems: rejoin the tail
    val pathStr = parts.drop(5).mkString("|")
    val fr = FileRef(Path.of(pathStr), digest, quick, PathFilter.parseCanonical(filterCanonical),
      revalidate)
    fr.revalidate match
      case Revalidate.Never => fr
      case _                =>
        if !fr.validate then throw StaleFileRefException(fr)
        fr
  end parse

  final class StaleFileRefException(val fileRef: FileRef)
      extends RuntimeException(s"FileRef signature no longer matches disk content: $fileRef")
end FileRef
