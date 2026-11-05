package cardinality.analysis

import scala.meta.*

import cardinality.analysis.inhabitation.*
import org.specs2.Specification
import org.specs2.execute.Result

class GroundTypesSpec extends Specification {
  import Inhabitation.Count
  import Natural.apply as N
  import NaturalSyntax.pow

  def is = s2"""
    Transparent fixed-width ground types
      an Int endomorphism is every function Int → Int              $intEndo
      an Int input splits a choice uniformly                       $intChoice
      ground goals are their literals                              $literals
      a byte endomorphism materializes exactly                     $byteEndo
      a char predicate is 2^(2^16)                                 $charPredicate
      sum payloads compose with the split                          $payloads
      producers take literal arguments                             $producers
      unit, absent and absurd goals                                $degenerate
      productive cycles over atoms stay ω                          $atomCycle
      parameter order does not change the form                     $orderInvariant
      wider words multiply exponents                               $wider
      floating point counts its equals classes                     $floats
    Countable ground (String)
      String goals are ω                                           $stringGoal
      a String input saturates 0 / 1 / ω                           $stringSplit
      qualified spellings name the same types                      $qualified
    Closed values
      closed callables split by their value space                  $closedBooleans
      a declared ground member is a supplied value                 $nullaryDeclared
      an infinite closed space saturates                           $closedInfinite
      an empty closed value makes the context absurd               $emptyClosed
      closed repeated arguments are transparent sequences          $closedRepeated
    Constructors read closed positions by exact value space
      a sum position is exact, not a rounded bound                 $optionPosition
      a colliding closed position is a value space, not functions  $collision
      a finite function position stays finite                      $finiteFunctionPosition
      an infinite closed position keeps the estimate's tier         $infinitePosition
      a mixed position names its own reason                        $mixed
    Boundary
      a ground-result callable on an atom is split by its literals $groundCallable
      a closed Int callable is never ω                             $closedCallable
      a var reads as the value it was initialized with             $mutable
      an export binds a ground name; a given import binds none     $exported
      a source alias still shadows the builtin                     $sourceShadow
      higher-order use of a ground domain stays unresolved         $higherOrder
      repeated atoms are eliminated by length                      $repeatedAtoms
  """

  private def inputs(sources: (String, String)*): List[MethodAnalysis.Input] =
    sources.toList.map((p, t) => MethodAnalysis.Input(p, dialects.Scala3(t).parse[Source].get))

  private def all(sources: (String, String)*) = MethodAnalysis.analyze(inputs(sources*))

  private def count(code: String, name: String = "f"): Count =
    all("Example.scala" -> code).find(_.name == name).get.count

  private def unresolvedOn(c: Count, text: String): Result =
    c must beLike { case Count.Unresolved(rs) => rs.exists(_.contains(text)) must beTrue }

  private val two32 = BigInt(2).pow(32)
  private val two37 = BigInt(2).pow(37)

  def intEndo = {
    val c = count("def f(x: Int): Int = x")
    (c === Count.Symbolic(pow(2, pow(2, 37)))).and(c.render === "2^(2^37)")
  }

  def intChoice =
    count("def f[A](n: Int, a: A, b: A): A = a") === Count.Symbolic(pow(2, pow(2, 32)))

  def literals =
    (count("def f: Int = 0") === Count.Finite(two32))
      .and(count("def f: Int = 0").render === "2^32")
      .and(count("def f: Long = 0") === Count.Finite(BigInt(2).pow(64)))
      .and(count("def f: Char = 'a'") === Count.Finite(65536))
      .and(count("def f: Short = 0") === Count.Finite(65536))
      .and(count("def f: Byte = 0") === Count.Finite(256))
      .and(count("def f: (Int, Int) = (0, 0)") === Count.Finite(BigInt(2).pow(64)))

  def byteEndo = {
    val c = count("def f(b: Byte): Byte = b")
    (c === Count.Finite(BigInt(2).pow(2048))).and(c.render === "2^2048")
  }

  def charPredicate = {
    val c = count("def f(c: Char): Boolean = true")
    (c === Count.Symbolic(pow(2, 65536))).and(c.render === "2^(2^16)")
  }

  def payloads =
    (count("def f(o: Option[Int]): Int = 0") === Count.Symbolic(pow(2, N(two37 + 32))))
      .and(
        count("def f(x: Int): Option[Int] = None") === Count.Symbolic(pow(two32 + 1, pow(2, 32)))
      )
      .and(
        count("def f(e: Either[Int, Boolean]): Boolean = true") ===
          Count.Symbolic(pow(2, N(two32 + 2)))
      )

  def producers =
    (count("def f[A](g: Int => A): A = g(0)") === Count.Finite(two32))
      .and(count("def f[A](g: Int => A, x: Int): A = g(x)") === Count.Symbolic(pow(2, pow(2, 37))))
      .and(
        count("def f[A](g: Int => A, h: Int => A): A = g(0)") === Count.Finite(BigInt(2).pow(33))
      )
      .and(
        count("def f[A](x: Int, g: Int => A, a: A): A = a") ===
          Count.Symbolic(pow(two32 + 1, pow(2, 32)))
      )

  def degenerate =
    (count("def f[A](n: Int, a: A): A = a") === Count.Finite(1))
      .and(count("def f[A](n: Int): A = ???") === Count.Finite(0))
      .and(count("def f(n: Int): Unit = ()") === Count.Finite(1))
      .and(count("def f(n: Int, z: Nothing): Int = n") === Count.Finite(1))

  def atomCycle =
    (count("def f[A](x: Int, step: A => A, a: A): A = a") === Count.Countable)
      .and(count("def f[A](step: (A, Int) => A, a: A): A = a") === Count.Countable)

  def orderInvariant = {
    val a = count("def f(x: Int, b: Byte): Int = x")
    val b = count("def f(b: Byte, x: Int): Int = x")
    (a === Count.Symbolic(pow(2, pow(2, 45)))).and(a === b).and(a.render === b.render)
  }

  def wider =
    (count("def f(x: Short, y: Short): Short = x") === Count.Symbolic(pow(2, pow(2, 36))))
      .and(count("def f(x: Long): Boolean = true") === Count.Symbolic(pow(2, pow(2, 64))))

  def floats = {
    // binary32 has 2^24 − 2 NaN patterns, binary64 2^53 − 2; `equals` makes each set one value.
    val float = two32 - BigInt(2).pow(24) + 3
    (count("def f: Float = 0f") === Count.Finite(BigInt("4278190083")))
      .and(float === BigInt("4278190083"))
      .and(count("def f: Double = 0.0") === Count.Finite(BigInt("18437736874454810627")))
      .and(count("def f(x: Float): Boolean = true") === Count.Symbolic(pow(2, N(float))))
  }

  def stringGoal =
    (count("def f: String = \"\"") === Count.Countable)
      .and(count("def f(s: String): String = s") === Count.Countable)
      .and(count("def f(b: Byte): String = \"\"") === Count.Countable)

  def stringSplit =
    (count("def f[A](s: String, a: A): A = a") === Count.Finite(1))
      .and(count("def f[A](s: String): A = ???") === Count.Finite(0))
      .and(count("def f(s: String): Unit = ()") === Count.Finite(1))
      .and(count("def f(s: String, z: Nothing): String = s") === Count.Finite(1))
      .and(count("def f[A](s: String, a: A, b: A): A = a") === Count.Countable)
      .and(count("def f(s: String): Boolean = true") === Count.Countable)
      .and(count("def f[A](s: String, g: String => A): A = g(s)") === Count.Countable)

  // `Predef.String` is relative to the root import of `scala.Predef`: a top-level package `Predef`
  // outside the sources outranks it (Scala 3.9.0 fixture), and could declare a `String` that yields
  // an A. Only the fully qualified spellings are the builtin.
  def qualified =
    (count("def f(x: scala.Int): Int = x") === count("def f(x: Int): Int = x"))
      .and(count("def f[A](x: java.lang.String, a: A, b: A): A = a") === Count.Countable)
      .and(
        unresolvedOn(
          count("def f[A](x: Predef.String, a: A): A = a"),
          "builtin name Predef.String may be shadowed by a top-level package outside the sources: Predef"
        )
      )
      .and(count("def f[A](x: scala.Predef.String, a: A): A = a") === Count.Finite(1))

  def closedBooleans =
    (count("def f(g: Boolean => Boolean): Boolean = g(true)") === Count.Finite(16))
      .and(count("def f(g: Boolean => Boolean, x: Boolean): Boolean = x") === Count.Finite(256))
      .and(count("def f(g: Boolean => Boolean): Boolean => Boolean = g") === Count.Finite(256))
      .and(count("def f[A](g: Boolean => Boolean, a: A, b: A): A = a") === Count.Finite(16))
      .and(count("def f[A](g: Unit => Unit, a: A, b: A): A = a") === Count.Finite(2))

  def nullaryDeclared =
    // No producer of B: 0, as for the reviewed CanGet.get, not an elimination reason.
    (count("trait V[B] { def head: B; def length: Int }", "V.head") === Count.Finite(0))
      .and(
        count("trait V { def n: Int; def pick: Int }", "V.pick") === Count.Symbolic(
          pow(2, pow(2, 37))
        )
      )

  def closedInfinite =
    (count("def f(p: String => Boolean): Boolean = p(\"\")") === Count.Countable)
      .and(count("def f[A](p: String => Boolean, a: A): A = a") === Count.Finite(1))

  def emptyClosed =
    (count("def f(g: Int => Nothing): Int = 0") === Count.Finite(1))
      .and(count("def f[A](g: Int => Nothing): A = ???") === Count.Finite(1))

  def closedRepeated =
    (count("def f(xs: Int*): Int = 0") === Count.Countable)
      .and(count("def f[A](xs: Int*)(a: A): A = a") === Count.Finite(1))
      .and(count("def f(xs: Boolean*): Boolean = true") === Count.Countable)
      .and(count("def f[A](g: (Int*) => A): A = g()") === Count.Countable)

  def optionPosition = {
    // 2^64 + 2^32, which the estimate rounds to the bound 2^65.
    val c = count("case class P3(x: Int, y: Option[Int])", "P3.<init>")
    (c === Count.Finite(BigInt("18446744078004518912"))).and(c.render === "18446744078004518912")
  }

  def collision = {
    def pkg(name: String) =
      s"package $name\nfinal case class Inner(b: Byte)\nfinal case class Outer(x: Long, i: Inner)\n" +
        "final case class C(n: String)\nfinal case class O(id: Long, c: C)"
    val rows = all("a.scala" -> pkg("ka"), "b.scala" -> pkg("kb"))
    // |Long|·|Inner| = 2^64 · 2^8, not 2^64 · (2^8)^(2^8).
    (rows.filter(_.name.endsWith(".Outer.<init>")).map(_.count).distinct === List(
      Count.Finite(BigInt(2).pow(72))
    )).and(rows.filter(_.name.endsWith(".O.<init>")).map(_.count.render).distinct === List("ω"))
  }

  def finiteFunctionPosition =
    // |Int|^|Int => Int| = (2^32)^(2^(2^37)) = 2^(2^(2^37 + 5)); the estimate says ω.
    count("case class F(f: (Int => Int) => Int)", "F.<init>") ===
      Count.Symbolic(pow(2, pow(2, N(two37 + 5))))

  def infinitePosition =
    (count("case class Reducer(run: String => String)", "Reducer.<init>").render === "ε₀")
      .and(count("case class Name(n: String, k: Int)", "Name.<init>").render === "ω")

  def mixed =
    unresolvedOn(
      count("case class M[A](p: (A, Int))", "M.<init>"),
      "constructor position mixes type parameters with transparent ground values: p"
    )

  // `g(a)` is eliminated once by its literals (capabilities C10): an Int-valued function of one Int,
  // (2^32)^(2^32) = 2^(2^37). With `h: Int => A` the arguments of `g` include `h(n)` for every
  // literal `n`, an argument space this does not enumerate. A String result splits into countably
  // many literals, and the definable functions of one String are countably many (register D2): ω.
  def groundCallable =
    (count("def f[A](g: A => Int, a: A): Int = g(a)") === Count.Symbolic(pow(2, pow(2, 37))))
      .and(
        unresolvedOn(
          count("def f[A](g: A => Int, h: Int => A, a: A): Int = g(a)"),
          "sum elimination over an argument space not enumerated: g"
        )
      )
      .and(count("def f[A](a: A, h: A => String): String = h(a)") === Count.Countable)

  def closedCallable =
    // Finite — the functionals of (g, x) — never the ω a producer cycle would claim.
    count("def f(g: Int => Int, x: Int): Int = g(x)") ===
      Count.Symbolic(pow(2, pow(2, N(two37 + 37))))

  // The model excludes mutation, so `n` is the 0 it holds: `get` has no Int input and may return
  // any of the 2^32 values, as `def get: Int` alone may.
  def mutable =
    count("class C { var n: Int = 0; def get: Int = n }", "C.get") === Count.Finite(
      BigInt(2).pow(32)
    )

  // `export foo.Int` defines `Int` as an alias of a declaration the sources do not supply; a given
  // selector brings the given instance `""` (a fixed value, no input) and no type name.
  def exported =
    unresolvedOn(
      count("export foo.Int\ndef f(x: Int): Int = x"),
      "declaration not supplied: foo.Int"
    )
      .and(
        // A given selector binds no type name: String => String over transparent countable data,
        // ω as for the plain signature.
        count(
          "object foo { given String = \"\" }\nimport foo.given String\ndef f(x: String): String = x"
        ) === Count.Countable
      )

  def sourceShadow =
    (count("type Int = Unit\ndef f(x: Int): Int = x") === Count.Finite(1))
      .and(
        count("object H { type Int = Boolean }\nimport H.Int\ndef f(x: Int): Int = x") === Count
          .Finite(4)
      )

  def higherOrder =
    unresolvedOn(
      count("def f[A](h: (Int => A) => A): A = ???"),
      "Higher-order application is unsupported"
    )

  // `fs` empty: only x (1); one f: x, f(x), f(f(x)), … — ω. Every length has 2^32 Int constants,
  // so c(0) ≥ 2 and c(1) ≥ 2: ω (finite programs bound it by ℵ₀).
  def repeatedAtoms =
    (count("def f[A](x: A)(fs: (A => A)*): A = x") === Count.Countable)
      .and(count("def f[A](xs: A*): Int = 0") === Count.Countable)

}
