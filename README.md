# Factum

*Latin: "a thing already made."*

**Factum** is a Scala 3 library for **persistent, incremental, content-addressed
computation**: you describe a pipeline as a typed task graph, and Factum caches every
step's results (Scala values and generated files alike) on disk, recomputing only the
steps whose inputs actually changed.

Build tools like Bazel, Buck2, Mill and sbt each have such a caching engine buried
inside them; Factum provides the same kind of engine as a plain, embeddable library.
It is **not** a build tool: no configuration DSL, no sandboxing, no process
orchestration. Your Scala program is the pipeline; Factum makes it incremental.

- **Type-safe.** Tasks are `Task[T]` values wired together with ordinary typed Scala.
  Serialization goes through a `Codec[T]` typeclass; cache keys are checked for
  cross-run stability instead of silently misbehaving.
- **Concurrency-safe.** Independent branches evaluate in parallel and each node runs
  at most once per evaluation. On disk, all writes are atomic and content-addressed,
  so concurrent processes sharing one cache directory cannot corrupt it.
- **Effect-safe.** The core is pure descriptions plus one explicit evaluation call,
  with no effect library required. An optional `cats-effect` module wraps evaluation
  in `IO`.
- **Files and folders are first-class.** External inputs are tracked by content
  digest. Generated folders are cached through filename filters and restored
  precisely, including cleanup of stale files.
- **Code is first-class too.** `CodeRef` digests a class/object together with its
  transitive code dependencies, so a cache entry can key off "did the code behind
  this computation actually change" instead of timestamps.
- **Early cutoff.** If a step re-runs but produces byte-identical output, downstream
  steps are not invalidated.

## Modules

| Module | Coordinates | Dependencies |
|---|---|---|
| Core | `io.github.dfiantworks::factum-core` | none (JDK only) |
| upickle codecs | `io.github.dfiantworks::factum-upickle` | upickle |
| cats-effect evaluator | `io.github.dfiantworks::factum-cats-effect` | cats-effect |

Not yet published to Maven Central; build locally with `./mill __.publishLocal`
(`mill.bat` on Windows).

```scala
// mill
def mvnDeps = Seq(mvn"io.github.dfiantworks::factum-core::0.1.0")
// sbt
libraryDependencies += "io.github.dfiantworks" %% "factum-core" % "0.1.0"
```

## Quick start

A report generator: read a data folder, compute statistics, render an HTML report
with a chart image. Each step caches; nothing recomputes unless its inputs changed.

```scala
import factum.*
import java.nio.file.Path

case class Stats(rows: Int, mean: Double)
given Codec[Stats] = Codec.fromString(
  s => s"${s.rows}|${s.mean}",
  str => { val p = str.split('\\|'); Stats(p(0).toInt, p(1).toDouble) }
)

val dataDir   = Path.of("data")
val reportDir = Path.of("out/report")

// track only the CSV files; editors' swap files and notes.txt are invisible
val data = Task.source(dataDir, PathFilter("**.csv"))

val stats = data.cached("stats", version = 1) { ref =>
  computeStats(ref.path) // your code
}

val report = stats.cachedWithFiles("render", extraKey = chartStyle) { s =>
  renderHtmlAndChart(s, reportDir) // writes report.html, chart.png, render.log
  (s, Vector(Output.Dir(reportDir, PathFilter("**.html", "**.png"))))
}

val evaluator = Evaluator(store.DiskStore(Path.of(".cache/factum")))
evaluator.eval(report)
```

What happens across runs:

- Second run, nothing changed: every step is a cache hit. If `report.html` or
  `chart.png` were deleted or edited meanwhile, they are restored byte-for-byte from
  the cache; the unmanaged `render.log` is left alone.
- A CSV changes: `stats` recomputes. If the statistics come out identical, `render`
  still hits the cache (early cutoff).
- A `notes.txt` appears in `data/`: no filter match, no invalidation, all hits.
- You change the rendering logic: bump `version = 2` on `render` to invalidate it.
- You change `chartStyle`: `render` recomputes, `stats` does not.

## Concepts

### Tasks

| Constructor | Meaning |
|---|---|
| `Task.pure(value)` | A constant, keyed by its encoded content. |
| `Task.input(name)(compute)` | Re-read every evaluation; downstream keys off the value, so an unchanged value still gives downstream hits. |
| `Task.source(path, filter, quick)` | External file/folder tracked by content digest. A missing path digests as a valid "absent" state. |
| `a.zip(b)` | Fan-in of two independent tasks, evaluated in parallel. |
| `Task.sequence(tasks)` | Fan-in of many homogeneous tasks. |
| `t.cached(name, version, extraKey)(f)` | A cached computation node. |
| `t.cachedWithFiles(name, ...)(f)` | Cached node that also declares generated files/folders. |
| `t.uncached(name)(f)` | Runs every evaluation (live side effects). |

The graph is applicative (static): its shape is fixed before evaluation, which is
what makes planning, parallelism and early cutoff tractable. Reuse task `val`s to
share upstream work; a diamond dependency computes once.

### Caching model

Factum keeps two kinds of records, both content-addressed with SHA-256:

- **CAS** (content-addressed store): blobs keyed by the digest of their bytes.
- **Action cache**: for each task execution, a record keyed by the digest of
  everything that went in: task name, `version`, evaluator `cacheVersion`, the
  *output* digests of its dependencies, and `extraKey`.

Keying off dependency *outputs* rather than inputs is what gives early cutoff:
identical output upstream means identical action keys downstream.

Three invalidation levers, from local to global:

1. `version` per task: bump when you change that task's logic.
2. `extraKey` per task: fold in data that affects the result but isn't a task input
   (tool versions, option sets). Values must be stable across JVM runs; lambdas,
   plain classes, `Set` and `Map` are rejected at runtime with an explanatory error.
3. `cacheVersion` per evaluator: global flush.

### Folder caching with filters

`PathFilter(includes, excludes)` uses glob patterns (`*` within a path segment,
`**` across segments, `?`, `[abc]`) matched case-sensitively against `/`-normalized
relative paths, so digests agree across operating systems. Excludes win over
includes.

The filter is part of the identity of everything it touches: the same folder seen
through two filters is two different cache entries, and changing a filter
invalidates exactly the tasks that used it. Non-matching files are invisible: they
cannot bust a cache, they are never stored, and restore never touches them.

For outputs, `Output.Dir(dir, filter)` defines a *managed scope*. On a cache hit
Factum syncs the scope to the cached manifest: missing or modified matching files
are re-materialized, stale matching files are deleted, everything else is left
untouched. Two tasks may manage disjoint filters over the same folder; overlapping
managed scopes are undefined behavior.

### Code-change detection with CodeRef

Timestamps are the wrong tool for "should this cached result survive a rebuild":
they change when nothing recompiled, and they miss dependencies compiled elsewhere.
`CodeRef` answers the real question at runtime, from the class files on disk:

```scala
val logic = CodeRef(classOf[ReportRenderer])   // digest of the code itself

val report = stats.cachedWithFiles("render", extraKey = (logic, chartStyle)) { s =>
  ...
}
```

The digest covers the class file, its sibling TASTy file (Scala 3 inline method
bodies live there, not in bytecode), and the transitive closure of every class it
statically references. Classes in directory classpath entries (your build output,
including sibling modules) are traversed individually; classes in jars fold the
whole jar in as one unit; JDK platform classes are skipped. Consequences:

- A rebuild that recompiles nothing relevant does not invalidate, no matter how
  many timestamps moved.
- A body change in a helper your code calls invalidates, even when your own class
  was not recompiled.
- A change to an unrelated class in the same output directory does not invalidate.

Blind spots, by design: reflection by name, resources read at runtime (use
`Task.source` for those), and environment state (fold it into `extraKey`).
Currently bytes are hashed raw, so formatting-only changes to files *inside the
closure* still invalidate; debug-info stripping is a planned refinement.

### Stores

```scala
import factum.store.*

val store = AggregateStore(   // layered: first hit wins, writes go to all
  MemoryStore(),              // fast L1
  DiskStore(cacheRoot)        // persistent, shareable between processes
)
```

`DiskStore` writes via temp file plus atomic move, and CAS filenames are pure
functions of content, so concurrent writers (threads or separate processes) are
benign by construction. A crash mid-write leaves only invisible temp files, never a
torn cache entry.

Maintenance: `DiskStore.gc(maxAgeMillis, maxTotalBytes)` drops old action records
and then sweeps CAS blobs no longer referenced by any surviving record.

### Evaluation

```scala
val ev = Evaluator(
  store,
  parallelism  = 8,                  // defaults to available processors
  cacheVersion = "1",
  listener     = new TaskListener:   // optional hooks
    override def onCacheHit(name: String): Unit = println(s"$name: hit")
)

ev.eval(task)         // cached evaluation
ev.evalUncached(task) // force recompute; still writes results (warms the cache)
Evaluator(store, cacheEnabled = false).eval(task) // bypass the cache entirely
```

Failures are never cached: an exception propagates out of `eval` and the failed
task re-runs on the next evaluation.

Cached values materialize lazily: on a fully-cached chain, only the values
actually demanded (the final result, or a recomputing node's input) are
deserialized; upstream hits contribute just their digests. Generated files are
always restored eagerly, so the on-disk state is complete even for nodes whose
values were never decoded.

### Effect integration

```scala
import factum.ce.*
val io: cats.effect.IO[Report] = evaluator.evalIO(report)
```

Evaluation runs on Factum's own pool via `IO.blocking`; the core never depends on
an effect library.

### Custom serialization

Core ships `Codec` instances for primitives, strings, options, eithers, tuples,
ordered collections and `FileRef`. For your own types either adapt existing
to/from-string functions with `Codec.fromString`, or derive from upickle:

```scala
import factum.upickle.given
import upickle.default.ReadWriter

case class Report(title: String, sections: List[String]) derives ReadWriter
// Codec[Report] is now available
```

Encoding must be deterministic: equal values should encode to equal bytes, since
early cutoff compares encoded output.

## Building

Built with [Mill](https://mill-build.org). Bootstrap scripts are checked in:

```
./mill __.test         # compile and test everything (mill.bat on Windows)
./mill __.reformat     # scalafmt
./mill __.publishLocal
```

## License

Apache License 2.0 (see [LICENSE](LICENSE)). Portions adapted from
[sbt](https://github.com/sbt/sbt) (Apache-2.0) and
[Mill](https://github.com/com-lihaoyi/mill) (MIT); see [NOTICE](NOTICE).
