package factum

import java.net.{JarURLConnection, URL}
import java.nio.file.{Files, Path, Paths}
import scala.collection.mutable

/** A reference to a class/object whose identity is the digest of its *code*: the class file, its
  * sibling TASTy file (when present), and the transitive closure of every class it statically
  * references.
  *
  * The twin of [[FileRef]] for code instead of data: use it as a cache key (`extraKey`) to
  * invalidate exactly when the code behind a computation changes, regardless of file timestamps or
  * unrelated recompilation.
  *
  * Traversal granularity:
  *   - classes in *directory* classpath entries (typical build output) are digested individually
  *     and their references followed;
  *   - classes in *jars* fold the whole jar in as one unit (jars change atomically with versions,
  *     so per-class granularity buys nothing);
  *   - JDK platform classes (`jrt:`) are skipped.
  *
  * Known blind spots, by design: reflection by name (`Class.forName("...")`), resources read at
  * runtime (track them with `Task.source`), and environment state (fold it into `extraKey`). The
  * closure over-approximates reachability: a reference anywhere in a class counts, even from
  * methods never called.
  *
  * Current limitation: class and TASTy bytes are hashed raw, so a comment or formatting change
  * *inside the closure* still changes the digest (debug info and TASTy positions shift). Changes to
  * files outside the closure, or rebuilds that do not recompile closure members, never invalidate.
  */
final case class CodeRef(className: String, digest: Digest):
  override def toString: String = CodeRef.render(this)

object CodeRef:
  /** @param traverseJar
    *   jars for which to follow per-class references instead of folding the whole jar in as one
    *   unit (default: none)
    * @param quickJars
    *   digest folded-in jars by (name, mtime, size) instead of full content
    */
  final case class Config(
      traverseJar: Path => Boolean = _ => false,
      quickJars: Boolean = true
  )

  def apply(cls: Class[?]): CodeRef = apply(cls, Config())

  def apply(cls: Class[?], config: Config): CodeRef =
    val loader = Option(cls.getClassLoader).getOrElse(ClassLoader.getSystemClassLoader)
    val b = Digest.Builder()
    b.updateString("CodeRef")
    b.updateString(cls.getName)
    val visitedClasses = mutable.Set.empty[String]
    val visitedTasty = mutable.Set.empty[String]
    val coarseJars = mutable.SortedMap.empty[String, Path] // file name -> path

    def classBytes(url: URL): Array[Byte] =
      val in = url.openStream()
      try in.readAllBytes()
      finally in.close()

    def jarPathOf(url: URL): Option[Path] =
      url.openConnection() match
        case jar: JarURLConnection => Some(Paths.get(jar.getJarFileURL.toURI))
        case _                     => None

    /** The sibling TASTy file carries code invisible in bytecode (inline method bodies expand at
      * call sites from TASTy, macro implementations, ...). Nested and companion classes share the
      * outermost definition's TASTy file.
      */
    def digestTasty(internalName: String): Unit =
      val candidates =
        val outer = internalName.takeWhile(_ != '$')
        if outer == internalName then List(internalName) else List(internalName, outer)
      candidates.iterator
        .map(_ + ".tasty")
        .filter(visitedTasty.add)
        .map(loader.getResource)
        .find(_ != null)
        .foreach { url =>
          b.updateString("tasty")
          b.updateBytes(classBytes(url))
        }

    def visit(internalName: String): Unit =
      if visitedClasses.add(internalName) then
        loader.getResource(internalName + ".class") match
          case null =>
            // generated at runtime or otherwise unresolvable: identity only
            b.updateString("missing")
            b.updateString(internalName)
          case url =>
            url.getProtocol match
              case "jrt" => () // JDK platform class
              case "jar" =>
                jarPathOf(url) match
                  case Some(jarPath) if !config.traverseJar(jarPath) =>
                    coarseJars(jarPath.getFileName.toString) = jarPath
                  case _ =>
                    digestAndRecurse(internalName, classBytes(url))
              case "file" =>
                digestAndRecurse(internalName, classBytes(url))
              case other =>
                b.updateString("external")
                b.updateString(s"$other:$internalName")

    def digestAndRecurse(internalName: String, bytes: Array[Byte]): Unit =
      b.updateString("class")
      b.updateString(internalName)
      b.updateBytes(bytes)
      digestTasty(internalName)
      // sorted for a deterministic traversal order
      ClassFile.referencedClasses(bytes).toVector.sorted.foreach(visit)

    visit(cls.getName.replace('.', '/'))

    // coarse jars last, sorted by file name (machine-independent, unlike full paths)
    for (name, path) <- coarseJars do
      b.updateString("jar")
      b.updateString(name)
      if config.quickJars then
        b.updateLong(Files.getLastModifiedTime(path).toMillis)
        b.updateLong(Files.size(path))
      else b.updateDigest(Digest.sha256File(path))

    CodeRef(cls.getName, b.result)
  end apply

  // --- wire format: "cref|<digest>|<className>" -------------------------------------

  def render(ref: CodeRef): String = s"cref|${ref.digest.asString}|${ref.className}"

  def parse(s: String): CodeRef =
    val parts = s.split('|')
    require(parts.length >= 3 && parts(0) == "cref", s"malformed CodeRef string: $s")
    CodeRef(parts.drop(2).mkString("|"), Digest(parts(1)))
end CodeRef
