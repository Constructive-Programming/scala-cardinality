//> using scala 3.8.2

// Total program cardinality before and after a pass, as one polynomial a·ε₀ + b·ω + n.
//
// Usage: scala-cli run report-diff.scala -- <before report.txt> <after report.txt> [--rows]
// Native: scala-cli --power package --native report-diff.scala -o report-diff
// Every row of both sections (implementations and stored values) is summed. `?` rows have no
// size and are counted beside the polynomial; `—` rows have none of their own. A bound `≤2^k`
// counts as 2^k. The finite part is exact while it fits in 64 bits, otherwise printed as a
// power of two. `--rows` also lists every row whose size changed, appeared (`·`) or left.

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

final case class Polynomial(eps: BigInt, omega: BigInt, logs: List[Double], unresolved: Int)

/** (section:name, size) per row; a repeated name is numbered in report order. */
def rows(path: String): List[(String, String)] =
  val lines = Files.readAllLines(Path.of(path)).asScala.toList
  val (_, _, out) = lines.foldLeft((Option.empty[String], Map.empty[String, Int], Vector.empty[(String, String)])) {
    case ((section, seen, out), line) =>
      if line.startsWith("Stored-value estimates") then (Some("stored"), seen, out)
      else if line.startsWith("Generic method") then (Some("signature"), seen, out)
      else
        section.filter(_ => line.nonEmpty && !line.startsWith(" ")).map(s => (s, line.split(" {2,}"))) match
          case Some((s, cols)) if cols.length >= 3 =>
            val key = s"$s:${cols(1)}"
            val n   = seen.getOrElse(key, 0) + 1
            (section, seen.updated(key, n), out :+ ((if n > 1 then s"$key #$n" else key), cols(0)))
          case _ => (section, seen, out)
  }
  out.toList

/** log2 of a finite term: `n`, `a^b`, `a^(b^c)`, a product of those, `≤` ignored. */
def log2Finite(term: String): Double =
  def log2(x: Double) = math.log(x) / math.log(2)
  term.replace("≤", "").replace("(lossy)", "").trim.split("·").toList.map { factor =>
    "\\d+".r.findAllIn(factor).map(_.toDouble).toList match
      case List(n)       => if n > 0 then log2(n) else Double.NegativeInfinity
      case List(a, b)    => b * log2(a)
      case List(a, b, c) => math.pow(b, c) * log2(a)
      case _             => sys.error(s"unread size: $term")
  }.sum

val Transfinite = """(\d*)(ε₀|ω)""".r

def polynomial(path: String): Polynomial =
  rows(path).map(_._2).foldLeft(Polynomial(0, 0, Nil, 0)) {
    case (p, "?") => p.copy(unresolved = p.unresolved + 1)
    case (p, "—") => p
    case (p, size) =>
      size.split(" \\+ ").map(_.trim).foldLeft(p) {
        case (q, Transfinite(k, "ε₀")) => q.copy(eps = q.eps + BigInt(if k.isEmpty then "1" else k))
        case (q, Transfinite(k, _))    => q.copy(omega = q.omega + BigInt(if k.isEmpty then "1" else k))
        case (q, term)                 => q.copy(logs = log2Finite(term) :: q.logs)
      }
  }

def render(p: Polynomial): String =
  val finite = p.logs.filter(_ > Double.NegativeInfinity)
  val n =
    if finite.isEmpty then "0"
    else
      val top = finite.max
      val e   = top + math.log(finite.map(l => math.pow(2, l - top)).sum) / math.log(2)
      if e < 64 then BigDecimal(finite.map(math.pow(2, _)).sum).setScale(0, BigDecimal.RoundingMode.HALF_EVEN).toString
      else if e < 1e6 then f"≈2^$e%.1f"
      else f"≈2^(2^${math.log(e) / math.log(2)}%.2f)"
  val parts = Option.when(p.eps != 0)(s"${p.eps}ε₀").toList ++ Option.when(p.omega != 0)(s"${p.omega}ω") :+ n
  parts.mkString(" + ") + s"   (${p.unresolved} rows ?)"

@main def reportDiff(args: String*): Unit =
  args.filterNot(_ == "--rows") match
    case Seq(before, after) =>
      println("before: " + render(polynomial(before)))
      println("after:  " + render(polynomial(after)))
      if args.contains("--rows") then
        val (old, now) = (rows(before).toMap, rows(after).toMap)
        (old.keySet ++ now.keySet).toList.sorted.foreach { key =>
          val (a, b) = (old.getOrElse(key, "·"), now.getOrElse(key, "·"))
          if a != b then
            val Array(section, name) = key.split(":", 2)
            println(f"  $section%-9s $a%16s → $b%-16s $name")
        }
    case _ => sys.exit { System.err.println("usage: report-diff <before report.txt> <after report.txt> [--rows]"); 2 }
