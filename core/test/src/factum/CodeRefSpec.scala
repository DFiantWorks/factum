package factum

import java.net.URLClassLoader
import java.nio.file.{Files, Path}
import java.util.jar.{JarEntry, JarOutputStream}

class CodeRefSpec extends munit.FunSuite:
  override def munitTimeout = scala.concurrent.duration.Duration(120, "s")

  val work = FunFixture[Path](
    setup = _ => Files.createTempDirectory("factum-coderef"),
    teardown = deleteRecursively
  )

  def deleteRecursively(p: Path): Unit =
    if Files.isDirectory(p) then
      val stream = Files.list(p)
      try stream.forEach(deleteRecursively)
      finally stream.close()
    Files.deleteIfExists(p)

  /** One source file per class, so editing one fixture never shifts positions or line numbers
    * inside another fixture's artifacts.
    */
  def writeFixtures(srcDir: Path, helperBody: String, bystanderBody: String): Vector[Path] =
    Files.createDirectories(srcDir)
    def write(name: String, content: String): Path =
      Files.writeString(srcDir.resolve(name), content)
    Vector(
      write("Helper.scala", s"object Helper { def greeting: String = $helperBody }"),
      write("Bystander.scala", s"object Bystander { def noise: String = $bystanderBody }"),
      write("Top.scala", "class Top { def run: String = Helper.greeting }")
    )

  // The forked test runner does not expose dependencies on java.class.path, so
  // -usejavacp cannot find the standard library; locate it via witness classes.
  lazy val stdlibClasspath: String =
    def entryOf(cls: Class[?]): String =
      Path.of(cls.getProtectionDomain.getCodeSource.getLocation.toURI).toString
    List(
      entryOf(Class.forName("scala.Predef$")), // scala-library (2.13 stdlib)
      entryOf(classOf[scala.util.NotGiven[?]]) // scala3-library
    ).distinct.mkString(java.io.File.pathSeparator)

  def compile(sources: Vector[Path], out: Path): Unit =
    Files.createDirectories(out)
    val args = Array("-d", out.toString, "-classpath", stdlibClasspath) ++ sources.map(_.toString)
    val reporter = dotty.tools.dotc.Main.process(args)
    assert(
      !reporter.hasErrors,
      s"fixture compilation failed:\n${reporter.allErrors.map(_.message).mkString("\n")}"
    )

  def codeRefOf(out: Path, className: String): CodeRef =
    val loader = URLClassLoader(Array(out.toUri.toURL), getClass.getClassLoader)
    try CodeRef(loader.loadClass(className))
    finally loader.close()

  work.test("identical sources digest identically across output directories"): work =>
    val sources = writeFixtures(work.resolve("src"), "\"hello\"", "\"zzz\"")
    compile(sources, work.resolve("out1"))
    compile(sources, work.resolve("out2"))
    assertEquals(codeRefOf(work.resolve("out1"), "Top"), codeRefOf(work.resolve("out2"), "Top"))

  work.test("a body change in a referenced dependency changes the digest"): work =>
    val src = work.resolve("src")
    compile(writeFixtures(src, "\"hello\"", "\"zzz\""), work.resolve("out1"))
    compile(writeFixtures(src, "\"HELLO CHANGED\"", "\"zzz\""), work.resolve("out2"))
    assertNotEquals(
      codeRefOf(work.resolve("out1"), "Top").digest,
      codeRefOf(work.resolve("out2"), "Top").digest
    )

  work.test("a change in an unreferenced class does not change the digest"): work =>
    val src = work.resolve("src")
    compile(writeFixtures(src, "\"hello\"", "\"zzz\""), work.resolve("out1"))
    compile(writeFixtures(src, "\"hello\"", "\"DIFFERENT NOISE\""), work.resolve("out2"))
    assertEquals(
      codeRefOf(work.resolve("out1"), "Top").digest,
      codeRefOf(work.resolve("out2"), "Top").digest
    )

  work.test("an inline method body change is caught (lives in TASTy, not bytecode)"): work =>
    val src = work.resolve("src")
    def fixtures(inlineBody: String) =
      Files.createDirectories(src)
      Vector(
        Files.writeString(
          src.resolve("Helper.scala"),
          s"object Helper { inline def greeting: String = $inlineBody }"
        ),
        Files.writeString(
          src.resolve("Top.scala"),
          "class Top { def run: String = \"top\" }" // does NOT call Helper
        ),
        Files.writeString(
          src.resolve("Mid.scala"),
          "object Mid { def viaHelper: String = Helper.greeting }"
        )
      )
    end fixtures
    compile(fixtures("\"a\""), work.resolve("out1"))
    compile(fixtures("\"b\""), work.resolve("out2"))
    // Mid's bytecode contains the expansion, but Helper's own class file has no
    // body for the inline def; Helper's TASTy carries it. Digest Helper directly:
    assertNotEquals(
      codeRefOf(work.resolve("out1"), "Helper$").digest,
      codeRefOf(work.resolve("out2"), "Helper$").digest
    )

  def mkJar(out: Path, jarPath: Path, mutateBystander: Boolean): Unit =
    val jos = JarOutputStream(Files.newOutputStream(jarPath))
    try
      FileRef.walkFiltered(out, PathFilter.all).foreach { (rel, file) =>
        jos.putNextEntry(JarEntry(rel))
        val bytes = Files.readAllBytes(file)
        // flip a byte in Bystander's class file only
        if mutateBystander && rel.startsWith("Bystander") && rel.endsWith(".class") then
          bytes(bytes.length - 1) = (bytes(bytes.length - 1) ^ 0x01).toByte
        jos.write(bytes)
        jos.closeEntry()
      }
    finally jos.close()

  def refOfJar(jarPath: Path): CodeRef =
    val loader = URLClassLoader(Array(jarPath.toUri.toURL), getClass.getClassLoader)
    try CodeRef(loader.loadClass("Top"))
    finally loader.close()

  work.test("jars fold in coarsely: any change inside the jar invalidates"): work =>
    val out = work.resolve("out")
    compile(writeFixtures(work.resolve("src"), "\"hello\"", "\"zzz\""), out)
    // identical file names in separate dirs, so only content differs
    val dir1 = Files.createDirectory(work.resolve("j1")).resolve("fixture.jar")
    val dir2 = Files.createDirectory(work.resolve("j2")).resolve("fixture.jar")
    mkJar(out, dir1, mutateBystander = false)
    mkJar(out, dir2, mutateBystander = true)
    assertNotEquals(refOfJar(dir1).digest, refOfJar(dir2).digest)

  work.test("non-repository jars are keyed by content: fresh names/mtimes do not invalidate"):
    work =>
      val out = work.resolve("out")
      compile(writeFixtures(work.resolve("src"), "\"hello\"", "\"zzz\""), out)
      // run-unique names, as an sbt background-run classpath snapshot would have
      val jar1 = work.resolve("snapshot-1744.jar")
      val jar2 = work.resolve("snapshot-1745.jar")
      mkJar(out, jar1, mutateBystander = false)
      Thread.sleep(10) // ensure a different mtime
      mkJar(out, jar2, mutateBystander = false)
      assertEquals(refOfJar(jar1).digest, refOfJar(jar2).digest)

  test("CodeRef over the library's own classes is deterministic and total"):
    val a = CodeRef(classOf[FileRef])
    val b = CodeRef(classOf[FileRef])
    assertEquals(a, b)
    assertEquals(a.className, "factum.FileRef")

  test("render/parse roundtrip, Codec and KeyHash stability"):
    val ref = CodeRef(classOf[PathFilter])
    assertEquals(CodeRef.parse(CodeRef.render(ref)), ref)
    val c = summon[Codec[CodeRef]]
    assertEquals(c.decode(c.encode(ref)), ref)
    assertEquals(KeyHash.digestOf(ref), KeyHash.digestOf(CodeRef(classOf[PathFilter])))

  test("constant-pool parser sees superclass and referenced types"):
    val loader = getClass.getClassLoader
    val bytes =
      val in = loader.getResourceAsStream("factum/CodeRefSpec.class")
      try in.readAllBytes()
      finally in.close()
    val refs = ClassFile.referencedClasses(bytes)
    assert(refs.contains("munit/FunSuite"), refs.toString)
    assert(refs.contains("factum/CodeRef"), refs.toString)
end CodeRefSpec
