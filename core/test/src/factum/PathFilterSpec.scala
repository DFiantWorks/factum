package factum

class PathFilterSpec extends munit.FunSuite:
  test("default matches everything"):
    assert(PathFilter.all.matches("a.txt"))
    assert(PathFilter.all.matches("deep/nested/file.bin"))

  test("star does not cross directory boundaries, double star does"):
    val flat = PathFilter("*.vhd")
    assert(flat.matches("top.vhd"))
    assert(!flat.matches("sub/top.vhd"))
    val deep = PathFilter("**.vhd")
    assert(deep.matches("top.vhd"))
    assert(deep.matches("sub/deeper/top.vhd"))
    assert(!deep.matches("top.sv"))

  test("question mark and character classes"):
    val f = PathFilter("file?.[st]v")
    assert(f.matches("file1.sv"))
    assert(f.matches("fileA.tv"))
    assert(!f.matches("file12.sv"))
    assert(!f.matches("file1.xv"))

  test("excludes take precedence"):
    val f = PathFilter(Seq("**"), Seq("work/**", "**.log"))
    assert(f.matches("rtl/top.vhd"))
    assert(!f.matches("work/lib.o"))
    assert(!f.matches("rtl/sim.log"))

  test("matching is case-sensitive on every platform"):
    val f = PathFilter("**.vhd")
    assert(f.matches("top.vhd"))
    assert(!f.matches("TOP.VHD"))

  test("dir pruning only for full double-star excludes"):
    val f = PathFilter(Seq("**"), Seq("work/**", "**.log"))
    assert(f.prunesDir("work"))
    assert(!f.prunesDir("rtl"))
    val g = PathFilter(Seq("**"), Seq("**/obj_dir/**"))
    assert(g.prunesDir("sub/obj_dir"))

  test("canonical is order-stable and parseable"):
    val a = PathFilter(Seq("**.b", "**.a"), Seq("y/**", "x/**"))
    val b = PathFilter(Seq("**.a", "**.b"), Seq("x/**", "y/**"))
    assertEquals(a.canonical, b.canonical)
    val parsed = PathFilter.parseCanonical(a.canonical)
    assertEquals(parsed.canonical, a.canonical)
    assertEquals(
      PathFilter.parseCanonical(PathFilter.all.canonical).canonical,
      PathFilter.all.canonical
    )

  test("regex metacharacters in filenames are literal"):
    val f = PathFilter("a+b(c).txt")
    assert(f.matches("a+b(c).txt"))
    assert(!f.matches("aab(c).txt"))
end PathFilterSpec
