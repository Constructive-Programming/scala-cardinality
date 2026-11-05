package cardinality.analysis.inhabitation

import org.specs2.mutable.Specification

class InhabitationSpec extends Specification {
  import Inhabitation.*
  import Shape.*
  import Count.*

  private val a = Atom("A")
  private val b = Atom("B")
  private val c = Atom("C")
  private val unit = Product(Nil)
  private val empty = Sum(Nil)
  private val x = Binding("x", a)
  private val y = Binding("y", a)
  private val int = Ground("Int", Card.exact(BigInt(2).pow(32)))
  private val string = Ground("String", Card.Omega)

  private def fn(id: String, args: List[Shape], out: Shape) =
    Binding(id, Function(args, out))

  "Inhabitation" should {
    "count constructions and preserve provenance" in {
      count(List(x, y), a) mustEqual Finite(2)
      count(List(x, x), a) mustEqual Finite(1)
      count(List(x, y), Product(List(a, a))) mustEqual Finite(4)
      count(List(Binding("pair", Product(List(a, a)))), a) mustEqual Finite(2)
      val f = fn("f", List(a), b)
      count(List(x, f, f), b) mustEqual Finite(1)
      count(List(x, f, fn("g", List(a), b)), b) mustEqual Finite(2)
      count(List(Binding("pair", Product(List(a, b)))), Product(List(a, b))) mustEqual Finite(1)
    }

    "use the least productive fixed point before considering cycles" in {
      val step = fn("step", List(a), a)
      count(List(step), a) mustEqual Finite(0)
      count(List(x, step), a) mustEqual Countable
      val cycle = List(fn("ab", List(a), b), fn("ba", List(b), a))
      count(cycle, b) mustEqual Finite(0)
      count(x :: cycle, b) mustEqual Countable
      count(x :: cycle, c) mustEqual Finite(0)
      count(List(x, fn("blocked", List(a, b), a)), a) mustEqual Finite(1)
      count(x :: cycle, Product(List(a, c))) mustEqual Finite(0)
      count(List(x, fn("blocked", List(Product(List(a, empty))), a)), a) mustEqual Finite(1)
    }

    "ignore irrelevant producers and collapse Unit extensionally" in {
      count(List(x, fn("loop", List(b), b)), a) mustEqual Finite(1)
      count(List(x, fn("loop", List(a), a)), unit) mustEqual Finite(1)
      count(Nil, Function(List(a), unit)) mustEqual Finite(1)
      count(Nil, empty) mustEqual Finite(0)
    }

    "introduce fresh binders and count multi-parameter and curried applications" in {
      count(Nil, Function(List(a), a)) mustEqual Finite(1)
      count(List(Binding("bound", a)), Function(List(a), a)) mustEqual Finite(2)
      count(
        List(fn("ab", List(a), b), fn("bc", List(b), c)),
        Function(List(a), c)
      ) mustEqual Finite(1)
      count(List(x, y, fn("f", List(a, a), b)), b) mustEqual Finite(4)
      count(List(x, y, fn("f", List(a), Function(List(a), b))), b) mustEqual Finite(4)
      count(List(x, fn("pair", List(a), Product(List(b, b)))), b) mustEqual Finite(2)
    }

    "split finite input sums and construct output sums" in {
      val bool = Sum(List(unit, unit))
      count(List(x, y, Binding("flag", bool)), a) mustEqual Finite(4)
      count(List(x), Sum(List(a, a))) mustEqual Finite(2)
      count(List(Binding("impossible", empty)), a) mustEqual Finite(1)
      count(List(x, Binding("pair", Product(List(unit, a)))), a) mustEqual Finite(2)
    }

    "split transparent ground values uniformly" in {
      count(List(Binding("x", int)), int) mustEqual Symbolic(
        NaturalSyntax.pow(2, NaturalSyntax.pow(2, 37))
      )
      count(List(x, Binding("n", int)), a) mustEqual Finite(1)
      count(List(Binding("n", int)), a) mustEqual Finite(0)
      count(List(Binding("s", string), x, y), a) mustEqual Countable
      count(List(Binding("s", string), x), a) mustEqual Finite(1)
      count(Nil, string) mustEqual Countable
      // A closed callable is a value: split, never applied as a producer.
      count(List(fn("g", List(int), int), Binding("n", int)), int) mustEqual
        Symbolic(NaturalSyntax.pow(2, NaturalSyntax.pow(2, Natural(BigInt(2).pow(37) + 37))))
      count(List(fn("never", List(int), empty)), a) mustEqual Finite(1)
    }

    // A sum result is cased once per argument (capabilities C10), and a ground result is a closed
    // sum of literals, eliminated the same way: the goal is a function of g's values.

    "expose unsupported operations and resource limits" in {
      count(List(fn("higher", List(Function(List(a), b)), b)), b) must
        beAnInstanceOf[Unresolved]
      // A sum-producing callable of a higher-order argument is not eliminated.
      count(List(fn("opaque", List(Function(List(a), a)), Sum(List(a, b)))), a) must
        beAnInstanceOf[Unresolved]
      count(Nil, a, maxStates = 0) must beAnInstanceOf[Unresolved]
      count(Nil, Product(List(a, b)), maxStates = 1) must beAnInstanceOf[Unresolved]
      Finite(BigInt(123)).render mustEqual "123"
      Countable.render mustEqual "ω"
      Unresolved(List("reason")).render mustEqual "?"
    }
  }
}
