package factum

import factum.store.*
import java.nio.file.{Files, Path}

class StoreSpec extends munit.FunSuite:
  val disk = FunFixture[DiskStore](
    setup = _ => DiskStore(Files.createTempDirectory("factum-store")),
    teardown = s => deleteRecursively(s.root)
  )

  def deleteRecursively(p: Path): Unit =
    if Files.isDirectory(p) then
      val stream = Files.list(p)
      try stream.forEach(deleteRecursively)
      finally stream.close()
    Files.deleteIfExists(p)

  def sampleResult(store: Store): ActionResult =
    val v = store.putBlob("the value".getBytes)
    val f = store.putBlob("file content".getBytes)
    ActionResult(
      v,
      Vector(FileEntry("C:/out dir/gen.vhd", f)),
      Vector(DirEntry(
        "C:/sandbox",
        PathFilter("**.vhd").canonical,
        Vector(FileEntry("rtl/top.vhd", f))
      ))
    )

  def storeContract(name: String, mk: () => Store)(using munit.Location): Unit =
    test(s"$name: blob put/get/contains"):
      val store = mk()
      val d = store.putBlob("hello".getBytes)
      assert(store.containsBlob(d))
      assertEquals(String(store.getBlob(d).get), "hello")
      assertEquals(store.getBlob(Digest.sha256("other")), None)
      assert(!store.containsBlob(Digest.sha256("other")))

    test(s"$name: blob put is idempotent"):
      val store = mk()
      assertEquals(store.putBlob("x".getBytes), store.putBlob("x".getBytes))

    test(s"$name: action roundtrip"):
      val store = mk()
      val result = sampleResult(store)
      val action = Digest.sha256("some action key")
      assertEquals(store.getAction(action), None)
      store.putAction(action, result)
      assertEquals(store.getAction(action), Some(result))
  end storeContract

  storeContract("memory", () => MemoryStore())
  storeContract("disk", () => DiskStore(Files.createTempDirectory("factum-store")))
  storeContract(
    "aggregate",
    () => AggregateStore(MemoryStore(), DiskStore(Files.createTempDirectory("factum-store")))
  )

  test("action result wire format is pinned"):
    val v = Digest.sha256("v")
    val f = Digest.sha256("f")
    val r = ActionResult(
      v,
      Vector(FileEntry("a/b.txt", f)),
      Vector(DirEntry("d", "i=**;e=", Vector(FileEntry("x.txt", f))))
    )
    val rendered = ActionResult.render(r)
    assert(rendered.startsWith("factum-ac v1\n"), rendered)
    assertEquals(ActionResult.parse(rendered), r)

  disk.test("blob roundtrips through files"): store =>
    val src = Files.createTempFile("factum", ".bin")
    Files.write(src, Array[Byte](1, 2, 3))
    val d = store.putBlobFromFile(src)
    val dest = src.getParent.resolve("factum-out/restored.bin")
    assert(store.getBlobToFile(d, dest))
    assert(Files.readAllBytes(dest).sameElements(Array[Byte](1, 2, 3)))

  disk.test("tmp staging leaves no visible partial files"): store =>
    store.putBlob("data".getBytes)
    val tmpFiles = Files.list(store.root.resolve("tmp"))
    try assertEquals(tmpFiles.count(), 0L)
    finally tmpFiles.close()

  disk.test("concurrent writers of identical content are safe"): store =>
    val bytes = "shared content".getBytes
    val threads = (1 to 8).map(_ =>
      Thread(() =>
        store.putBlob(bytes); ()
      )
    )
    threads.foreach(_.start())
    threads.foreach(_.join())
    assertEquals(String(store.getBlob(Digest.sha256(bytes)).get), "shared content")

  disk.test("aggregate layers: first hit wins, puts write through"): store =>
    val mem = MemoryStore()
    val agg = AggregateStore(mem, store)
    val d = agg.putBlob("layered".getBytes)
    assert(mem.containsBlob(d))
    assert(store.containsBlob(d))
    // present only in the second layer -> still found
    val dOnly = store.putBlob("disk only".getBytes)
    assertEquals(String(agg.getBlob(dOnly).get), "disk only")

  disk.test("gc removes aged actions and unreachable blobs"): store =>
    val keepBlob = store.putBlob("keep".getBytes)
    val dropBlob = store.putBlob("drop".getBytes)
    val keepAction = Digest.sha256("keep-action")
    val dropAction = Digest.sha256("drop-action")
    store.putAction(keepAction, ActionResult(keepBlob, Vector.empty, Vector.empty))
    store.putAction(dropAction, ActionResult(dropBlob, Vector.empty, Vector.empty))
    // age the drop action far into the past
    val acFile = Files.list(store.root.resolve("ac")).filter(
      _.getFileName.toString.startsWith(dropAction.hexHash)
    ).findFirst.get
    Files.setLastModifiedTime(
      acFile,
      java.nio.file.attribute.FileTime.fromMillis(System.currentTimeMillis() - 1000000)
    )
    store.gc(maxAgeMillis = 500000)
    assertEquals(store.getAction(dropAction), None)
    assert(store.getAction(keepAction).isDefined)
    assert(store.containsBlob(keepBlob))
    assert(!store.containsBlob(dropBlob))
end StoreSpec
