package factum

import scala.util.matching.Regex

/** Filename filter for folder digesting/caching/restoring.
  *
  * Patterns use glob syntax (`**` crosses directory boundaries, `*` matches within a segment, `?`
  * matches one character, `[abc]`/`[a-z]` character classes) and are matched against the path
  * *relative* to the folder root, `/`-normalized. Matching is always **case-sensitive**, regardless
  * of the underlying filesystem, so a tree digests identically on Windows and Linux.
  *
  * Excludes take precedence over includes.
  */
final case class PathFilter(
    includes: Seq[String] = Seq("**"),
    excludes: Seq[String] = Seq.empty
):
  require(includes.nonEmpty, "PathFilter needs at least one include pattern")

  private lazy val includeRes: Seq[Regex] = includes.map(PathFilter.globToRegex)
  private lazy val excludeRes: Seq[Regex] = excludes.map(PathFilter.globToRegex)

  /** Whether a file at `relPath` (relative, `/`-separated) is selected. */
  def matches(relPath: String): Boolean =
    !excludeRes.exists(_.matches(relPath)) && includeRes.exists(_.matches(relPath))

  /** Whether the walk may skip the entire directory at `relDir`: true when some double-star exclude
    * anchored at a directory prefix covers it (excludes always win, so nothing under it can match).
    */
  def prunesDir(relDir: String): Boolean =
    excludes.exists { p =>
      p.endsWith("/**") && {
        val prefix = p.dropRight(3)
        relDir == prefix || PathFilter.globToRegex(prefix).matches(relDir)
      }
    }

  /** Stable string form, folded into every digest that used this filter. Includes/excludes are
    * OR-semantics, so sorting does not change meaning.
    */
  def canonical: String =
    s"i=${includes.sorted.mkString(",")};e=${excludes.sorted.mkString(",")}"
end PathFilter

object PathFilter:
  val all: PathFilter = PathFilter()

  def apply(first: String, rest: String*): PathFilter = PathFilter(first +: rest.toSeq)

  /** Inverse of [[PathFilter.canonical]]: parse "i=a,b;e=c,d". */
  def parseCanonical(canonical: String): PathFilter =
    val parts = canonical.split(';')
    def patterns(part: String): Seq[String] =
      val body = part.drop(2)
      if body.isEmpty then Seq.empty else body.split(',').toSeq
    val includes = patterns(parts(0))
    val excludes = if parts.length > 1 then patterns(parts(1)) else Seq.empty
    PathFilter(if includes.isEmpty then Seq("**") else includes, excludes)

  /** Case-sensitive glob → regex over `/`-separated relative paths. */
  private[factum] def globToRegex(glob: String): Regex =
    val sb = StringBuilder()
    var i = 0
    while i < glob.length do
      glob(i) match
        case '*' =>
          if i + 1 < glob.length && glob(i + 1) == '*' then
            sb ++= ".*"
            i += 1
          else sb ++= "[^/]*"
        case '?' => sb ++= "[^/]"
        case '[' =>
          val close = glob.indexOf(']', i + 1)
          require(close > i, s"unclosed character class in glob: $glob")
          val body0 = glob.substring(i + 1, close)
          val body = if body0.startsWith("!") then "^" + body0.drop(1) else body0
          sb ++= s"[$body]"
          i = close
        case c if "\\.^$+{}()|".contains(c) => sb ++= s"\\$c"
        case c                              => sb += c
      end match
      i += 1
    end while
    sb.toString.r
  end globToRegex
end PathFilter
