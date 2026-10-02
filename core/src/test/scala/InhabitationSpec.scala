package cardinality

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

    "retain exact arithmetic and bound arithmetic output" in {
      count(List(x, y), Product(List.fill(100)(a))) mustEqual Finite(BigInt(2).pow(100))
      count(List(x, y), Product(List.fill(65537)(a))) must beAnInstanceOf[Unresolved]
    }

    "expose unsupported operations and resource limits" in {
      count(List(fn("higher", List(Function(List(a), a)), b)), b) must
        beAnInstanceOf[Unresolved]
      count(List(fn("opaque", Nil, Sum(List(a, b)))), a) must beAnInstanceOf[Unresolved]
      count(Nil, a, maxStates = 0) must beAnInstanceOf[Unresolved]
      count(Nil, Product(List(a, b)), maxStates = 1) must beAnInstanceOf[Unresolved]
      Finite(BigInt(123)).render mustEqual "123"
      Countable.render mustEqual "ω"
      Unresolved(List("reason")).render mustEqual "?"
    }
  }
}
