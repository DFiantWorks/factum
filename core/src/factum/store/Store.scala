package factum
package store

import java.nio.file.Path

/** A single generated file inside an [[ActionResult]]: where it lives on disk (absolute for
  * `FileEntry`, relative to its dir for entries of [[DirEntry]]) and the CAS digest of its content.
  */
final case class FileEntry(path: String, digest: Digest)

/** A cached folder is a *manifest*, not a zip: per-file dedup in the CAS, selective restore, and
  * cheap per-file "already up to date" checks. Only files matching the filter belong to the managed
  * scope.
  */
final case class DirEntry(dir: String, filterCanonical: String, entries: Vector[FileEntry])

/** The cached outcome of one task action: the CAS digest of the serialized value plus the manifests
  * of any declared output files/folders.
  */
final case class ActionResult(
    valueDigest: Digest,
    outputFiles: Vector[FileEntry],
    outputDirs: Vector[DirEntry]
)

/** Content-addressed blob storage: digest → bytes. Writes are idempotent (the same digest always
  * maps to the same bytes), which is what makes concurrent writers - including ones in different
  * processes - benign.
  */
trait BlobStore:
  def putBlob(bytes: Array[Byte]): Digest
  def putBlobFromFile(file: Path): Digest
  def getBlob(digest: Digest): Option[Array[Byte]]

  /** Materialize a blob to `dest`, creating parent dirs. False if the blob is missing. */
  def getBlobToFile(digest: Digest, dest: Path): Boolean
  def containsBlob(digest: Digest): Boolean

/** Action-cache storage: action digest → [[ActionResult]]. */
trait ActionStore:
  def getAction(action: Digest): Option[ActionResult]
  def putAction(action: Digest, result: ActionResult): Unit

trait Store extends BlobStore, ActionStore

object ActionResult:
  // Line-based wire format, pinned by StoreSpec. Paths and filters are
  // base64url-encoded so whitespace and separators cannot corrupt a record.
  private val b64e = java.util.Base64.getUrlEncoder.withoutPadding
  private val b64d = java.util.Base64.getUrlDecoder
  private def enc(s: String): String =
    b64e.encodeToString(s.getBytes(java.nio.charset.StandardCharsets.UTF_8))
  private def dec(s: String): String =
    String(b64d.decode(s), java.nio.charset.StandardCharsets.UTF_8)

  def render(r: ActionResult): String =
    val sb = StringBuilder()
    sb ++= "factum-ac v1\n"
    sb ++= s"value ${r.valueDigest.asString}\n"
    for f <- r.outputFiles do sb ++= s"file ${f.digest.asString} ${enc(f.path)}\n"
    for d <- r.outputDirs do
      sb ++= s"dir ${enc(d.dir)} ${enc(d.filterCanonical)} ${d.entries.length}\n"
      for e <- d.entries do sb ++= s"entry ${e.digest.asString} ${enc(e.path)}\n"
    sb.toString

  def parse(s: String): ActionResult =
    val lines = s.linesIterator.buffered
    require(lines.hasNext && lines.next() == "factum-ac v1", "unknown action-result format")
    val valueLine = lines.next().split(' ')
    require(valueLine(0) == "value", "expected value line")
    val valueDigest = Digest(valueLine(1))
    val files = Vector.newBuilder[FileEntry]
    val dirs = Vector.newBuilder[DirEntry]
    while lines.hasNext do
      val parts = lines.next().split(' ')
      parts(0) match
        case "file" => files += FileEntry(dec(parts(2)), Digest(parts(1)))
        case "dir"  =>
          val n = parts(3).toInt
          val entries = Vector.fill(n) {
            val e = lines.next().split(' ')
            require(e(0) == "entry", "expected entry line")
            FileEntry(dec(e(2)), Digest(e(1)))
          }
          dirs += DirEntry(dec(parts(1)), dec(parts(2)), entries)
        case ""    => // trailing blank line
        case other => throw IllegalArgumentException(s"unknown action-result line: $other")
    ActionResult(valueDigest, files.result(), dirs.result())
  end parse
end ActionResult
