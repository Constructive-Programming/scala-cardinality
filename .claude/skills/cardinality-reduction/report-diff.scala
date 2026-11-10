//> using scala 3.8.2

// Total program cardinality before and after a pass, as one polynomial a·ε₀ + b·ω + n.
//
// Usage: scala-cli run report-diff.scala -- <before report.txt> <after report.txt> [--rows]
// Native: scala-cli --power package --native report-diff.scala -o report-diff
// Every row of both sections (implementations and stored values) is summed. `?` rows have no
// size and are counted beside the polynomial; `—` rows have none of their own. A bound `≤2^k`
// counts as 2^k. The finite part is exact up to 2^53, otherwise printed as a tower of twos
// (≈2^(2^103.00)). `--rows` also lists every row whose size changed, appeared (`·`) or left;
// `--check` runs the arithmetic's self-check.

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/** A finite size as a tower of `height` twos over `top`: 2^(2^(…^top)). Sizes here reach
  * 2^(2^(2^37)), far past a Double and past any BigInt worth building. Canonical form: `top` at
  * most 2^53 (so an integer is exact), and above 53 when `height` is positive.
  */
final case class Tower(height: Int, top: Double):
  def isZero: Boolean = height == 0 && top == 0
  def log2: Tower     = if height > 0 then Tower(height - 1, top) else Tower(0, lg(top))
  def exp2: Tower     = Tower.normal(height + 1, top)

  def +(that: Tower): Tower =
    val (hi, lo) = if Tower.ordering.gteq(this, that) then (this, that) else (that, this)
    hi.height match
      case 0 => Tower.normal(0, hi.top + lo.top)
      // 2^x + 2^y = 2^(x + log2(1 + 2^(y - x)))
      case 1 => Tower.normal(1, hi.top + lg(1 + math.pow(2, (if lo.height == 1 then lo.top else lg(lo.top)) - hi.top)))
      // ponytail: above 2^(2^53) the smaller summand moves no printed digit, so it is dropped
      case _ => hi

  def *(that: Tower): Tower = if isZero || that.isZero then Tower.Zero else (log2 + that.log2).exp2
  def ^(that: Tower): Tower = if that.isZero then Tower.One else if isZero then Tower.Zero else (that * log2).exp2

  def render: String =
    if height == 0 then BigDecimal(top).setScale(0, BigDecimal.RoundingMode.HALF_EVEN).toString
    else "≈" + "2^(" * (height - 1) + f"2^$top%.2f" + ")" * (height - 1)

object Tower:
  val Zero: Tower = Tower(0, 0)
  val One: Tower  = Tower(0, 1)
  private val Cap = math.pow(2, 53)

  given ordering: Ordering[Tower] = Ordering.by(t => (t.height, t.top))

  def normal(height: Int, top: Double): Tower =
    if top > Cap then normal(height + 1, lg(top))
    else if height > 0 && top <= 53 then normal(height - 1, math.pow(2, top))
    else Tower(height, top)

  /** A finite size as the report prints it: numbers, `^`, `·`, `+` and parentheses; `≤` and
    * `(lossy)` are ignored.
    */
  def parse(text: String): Tower =
    val src = text.replace("≤", "").replace("(lossy)", "").filterNot(_.isWhitespace)
    var at  = 0
    def eat(c: Char): Boolean = at < src.length && src(at) == c && { at += 1; true }
    def sum(): Tower =
      var left = product()
      while eat('+') do left = left + product()
      left
    def product(): Tower =
      var left = power()
      while eat('·') do left = left * power()
      left
    def power(): Tower =
      val base = atom()
      if eat('^') then base ^ power() else base
    def atom(): Tower =
      if eat('(') then
        val inner = sum()
        if !eat(')') then sys.error(s"unread size: $text")
        inner
      else
        val digits = src.drop(at).takeWhile(_.isDigit)
        if digits.isEmpty then sys.error(s"unread size: $text")
        at += digits.length
        normal(0, BigDecimal(digits).toDouble)
    val result = sum()
    if at != src.length then sys.error(s"unread size: $text")
    result

def lg(x: Double): Double = math.log(x) / math.log(2)

final case class Polynomial(eps: BigInt, omega: BigInt, finite: Tower, unresolved: Int)

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

val Transfinite = """(\d*)(ε₀|ω)""".r

def polynomial(path: String): Polynomial =
  rows(path).map(_._2).foldLeft(Polynomial(0, 0, Tower.Zero, 0)) {
    case (p, "?") => p.copy(unresolved = p.unresolved + 1)
    case (p, "—") => p
    case (p, size) =>
      size.split(" \\+ ").map(_.trim).foldLeft(p) {
        case (q, Transfinite(k, "ε₀")) => q.copy(eps = q.eps + BigInt(if k.isEmpty then "1" else k))
        case (q, Transfinite(k, _))    => q.copy(omega = q.omega + BigInt(if k.isEmpty then "1" else k))
        case (q, term)                 => q.copy(finite = q.finite + Tower.parse(term))
      }
  }

def render(p: Polynomial): String =
  val parts = Option.when(p.eps != 0)(s"${p.eps}ε₀").toList ++ Option.when(p.omega != 0)(s"${p.omega}ω") :+ p.finite.render
  parts.mkString(" + ") + s"   (${p.unresolved} rows ?)"

// The smallest check that fails if the tower arithmetic breaks.
def selfCheck(): Unit =
  def shows(size: String, rendered: String): Unit =
    val got = Tower.parse(size).render
    assert(got == rendered, s"$size renders $got, not $rendered")
  shows("2^32 + 2", "4294967298")
  shows("3·4", "12")
  shows("2^320", "≈2^320.00")
  shows("2^(2^103)", "≈2^(2^103.00)")
  shows("2^(2^33)·4294967297^(2^32)", "≈2^146028888065.44")
  shows("2^(2^32·3^(2^32)·4294967297^(2^32))", "≈2^(2^144246315611.43)")
  shows("2^(2^103) + 2^(2^102)", "≈2^(2^103.00)")

@main def reportDiff(args: String*): Unit =
  args.filterNot(_ == "--rows") match
    case Seq("--check") => selfCheck()
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
