package factum
package store

import java.nio.file.Path

/** Layered store: `get` returns the first hit in order, `put` writes through to all layers. Typical
  * use: `AggregateStore(MemoryStore(), DiskStore(dir))`. (Adapted design: sbt's
  * AggregateActionCacheStore, Apache-2.0.)
  */
final class AggregateStore(stores: Store*) extends Store:
  require(stores.nonEmpty, "AggregateStore needs at least one underlying store")

  def putBlob(bytes: Array[Byte]): Digest =
    stores.map(_.putBlob(bytes)).head

  def putBlobFromFile(file: Path): Digest =
    stores.map(_.putBlobFromFile(file)).head

  def getBlob(digest: Digest): Option[Array[Byte]] =
    stores.iterator.flatMap(_.getBlob(digest)).nextOption()

  def getBlobToFile(digest: Digest, dest: Path): Boolean =
    stores.exists(_.getBlobToFile(digest, dest))

  def containsBlob(digest: Digest): Boolean = stores.exists(_.containsBlob(digest))

  def getAction(action: Digest): Option[ActionResult] =
    stores.iterator.flatMap(_.getAction(action)).nextOption()

  def putAction(action: Digest, result: ActionResult): Unit =
    stores.foreach(_.putAction(action, result))
end AggregateStore
