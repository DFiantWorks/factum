package factum
package store

import java.nio.file.{Files, Path}
import java.util.concurrent.ConcurrentHashMap

/** In-memory store for tests and as a fast L1 in front of a DiskStore. */
final class MemoryStore extends Store:
  private val blobs = ConcurrentHashMap[String, Array[Byte]]()
  private val actions = ConcurrentHashMap[String, ActionResult]()

  def putBlob(bytes: Array[Byte]): Digest =
    val d = Digest.sha256(bytes)
    blobs.putIfAbsent(d.asString, bytes.clone)
    d

  def putBlobFromFile(file: Path): Digest = putBlob(Files.readAllBytes(file))

  def getBlob(digest: Digest): Option[Array[Byte]] =
    Option(blobs.get(digest.asString)).map(_.clone)

  def getBlobToFile(digest: Digest, dest: Path): Boolean =
    getBlob(digest) match
      case Some(bytes) =>
        if dest.getParent != null then Files.createDirectories(dest.getParent)
        Files.write(dest, bytes)
        true
      case None => false

  def containsBlob(digest: Digest): Boolean = blobs.containsKey(digest.asString)

  def getAction(action: Digest): Option[ActionResult] = Option(actions.get(action.asString))

  def putAction(action: Digest, result: ActionResult): Unit =
    actions.put(action.asString, result)
end MemoryStore
