package cardinality.analysis.inhabitation

import scala.util.Random

import org.specs2.mutable.Specification

class NaturalSpec extends Specification {
  import Natural.*
  import NaturalSyntax.*

  private def n(value: BigInt): Natural = Natural(value)
  private val two37 = BigInt(2).pow(37)

  "Natural" should {
    "keep every value that fits a literal as a literal" in {
      (pow(4, 16) mustEqual Lit(BigInt(2).pow(32)))
        .and(pow(2, 65535) mustEqual Lit(BigInt(2).pow(65535)))
        .and(pow(2, 65536) mustEqual Pow(Lit(2), Lit(65536)))
        .and(n(BigInt(2).pow(65535)) * n(2) mustEqual Pow(Lit(2), Lit(65536)))
        .and(pow(2, 65536).toBigInt must beNone)
        .and(n(12).toBigInt must beSome(BigInt(12)))
    }

    "reduce literal bases to their roots and merge equal bases" in {
      (pow(4, 40000) mustEqual pow(2, 80000))
        .and(pow(9, 40000) mustEqual pow(3, 80000))
        .and(pow(12, 30000) mustEqual (pow(2, 60000) * pow(3, 30000)))
        .and((pow(2, pow(2, 37)) ^ n(256)) mustEqual pow(2, pow(2, 45)))
        .and((n(BigInt(2).pow(32)) * pow(2, pow(2, 37))) mustEqual pow(2, n(two37 + 32)))
        .and((pow(2, pow(2, 37)) * pow(2, pow(2, 37))) mustEqual pow(2, pow(2, 38)))
        .and((n(3) * pow(3, 50000)) mustEqual pow(3, 50001))
    }

    "merge like terms of a sum" in {
      val big = pow(2, pow(2, 37))
      ((big + big) mustEqual pow(2, n(two37 + 1)))
        .and((n(3) * big + big) mustEqual pow(2, n(two37 + 2)))
        .and((big + n(2)) mustEqual Sum(List(big, Lit(2))))
        .and((n(2) + big + n(5)) mustEqual Sum(List(big, Lit(7))))
    }

    "answer zero and one exactly" in {
      val big = pow(2, pow(2, 37))
      (Zero.isZero must beTrue)
        .and(One.isOne must beTrue)
        .and((big * Zero) mustEqual Zero)
        .and((Zero * big) mustEqual Zero)
        .and((big * One) mustEqual big)
        .and((One ^ big) mustEqual One)
        .and((big ^ Zero) mustEqual One)
        .and((Zero ^ big) mustEqual Zero)
        .and((big ^ One) mustEqual big)
        .and(big.isZero must beFalse)
        .and(big.isOne must beFalse)
        .and((big + Zero) mustEqual big)
    }

    "render deterministically" in {
      (pow(2, pow(2, 37)).render mustEqual "2^(2^37)")
        .and(pow(BigInt(2).pow(32) + 1, pow(2, 32)).render mustEqual "4294967297^(2^32)")
        .and(pow(2, n(two37 + 32)).render mustEqual "2^137438953504")
        .and(pow(2, pow(2, n(two37 + 37))).render mustEqual "2^(2^137438953509)")
        .and(pow(2, 65536).render mustEqual "2^(2^16)")
        .and(n(255).render mustEqual "255")
        .and(n(256).render mustEqual "2^8")
        .and(n(BigInt(2).pow(32)).render mustEqual "2^32")
        .and(n((BigInt(2).pow(32) + 1).pow(3)).render mustEqual "79228162569604569827557507073")
        .and(n((BigInt(2).pow(32) + 1).pow(4)).render mustEqual "4294967297^4")
        .and(n(BigInt(3) * BigInt(2).pow(200)).render mustEqual "3·2^200")
        .and(n((BigInt(2).pow(32) + 1).pow(64) * 4).render mustEqual "4294967297^64·4")
        // Rendering keeps the normal form's root policy: an odd part wider than 4,096 bits is its
        // own root, so it prints in digits.
        .and(
          n((BigInt(2).pow(32) + 1).pow(256) * 4).render mustEqual
            s"${(BigInt(2).pow(32) + 1).pow(256)}·4"
        )
        .and(n(BigInt(2).pow(101) + 1).render mustEqual (BigInt(2).pow(101) + 1).toString)
        .and(Inhabitation.Count.Finite(BigInt(2).pow(2048)).render mustEqual "2^2048")
        .and((pow(2, pow(2, 37)) + n(2)).render mustEqual "2^(2^37) + 2")
        .and((n(3) * pow(2, pow(2, 37))).render mustEqual "3·2^(2^37)")
        .and(
          ((pow(2, pow(2, 37)) + n(1)) ^ n(BigInt(2).pow(32))).render mustEqual
            "(2^(2^37) + 1)^(2^32)"
        )
        .and(pow(2, 65536).nodes mustEqual 3)
        .and(pow(2, pow(2, 37)).height mustEqual 1)
    }

    "bound the representation, not the magnitude" in {
      def budget(limit: String)(value: => Natural) =
        value must throwA[Exceeded].like { case e => e.getMessage must contain(limit) }
      val terms = (0 until 300).map(k => pow(2, n(two37 + k)))
      budget("nodes")(terms.reduce(_ + _))
        .and(budget("power tower")((1 to 9).foldLeft(n(65536))((t, _) => pow(2, t))))
        .and((1 to 8).foldLeft(n(65536))((t, _) => pow(2, t)).height mustEqual 8)
        .and(
          // An odd product wider than a literal has no power of two to carry it.
          budget("literal")(n(BigInt(3).pow(41000)) * n(BigInt(3).pow(1000)))
        )
        .and(pow(3, 42000) mustEqual Pow(Lit(3), Lit(42000)))
    }

    "agree with exact integers where they fit" in {
      val random = new Random(20261031)
      val checks = (1 to 400).map { _ =>
        val (a, b) = (BigInt(random.nextInt(1 << 20)), BigInt(random.nextInt(1 << 20)))
        val e = BigInt(random.nextInt(40))
        (n(a) + n(b) mustEqual Lit(a + b))
          .and(n(a) * n(b) mustEqual Lit(a * b))
          .and((n(a) ^ n(e)) mustEqual Lit(a.pow(e.toInt)))
      }
      checks.reduce(_ and _)
    }

    "be commutative and associative on symbolic values" in {
      val random = new Random(7)
      val pool = List(
        n(2),
        n(3),
        n(BigInt(2).pow(32)),
        n(BigInt(2).pow(32) + 1),
        pow(2, pow(2, 37)),
        pow(BigInt(2).pow(32) + 1, pow(2, 32)),
        pow(3, 50000),
        pow(2, pow(2, 37)) + n(1)
      )
      def pick() = pool(random.nextInt(pool.size))
      val checks = (1 to 200).map { _ =>
        val (a, b, c) = (pick(), pick(), pick())
        ((a + b) mustEqual (b + a))
          .and((a * b) mustEqual (b * a))
          .and(((a + b) + c) mustEqual (a + (b + c)))
          .and(((a * b) * c) mustEqual (a * (b * c)))
          .and(((a ^ n(3)) ^ n(5)) mustEqual (a ^ n(15)))
      }
      checks.reduce(_ and _)
    }
  }

  "Card" should {
    "saturate the finite-program way" in {
      import Card.*
      val many = Card.exact(5)
      ((Omega * zero) mustEqual zero)
        .and((zero * Omega) mustEqual zero)
        .and((Omega * many) mustEqual Omega)
        .and((many + Omega) mustEqual Omega)
        .and((zero ^ Omega) mustEqual zero)
        .and((one ^ Omega) mustEqual one)
        .and((many ^ Omega) mustEqual Omega)
        .and((Omega ^ zero) mustEqual one)
        .and((Omega ^ many) mustEqual Omega)
        .and((many ^ Card.exact(2)) mustEqual Card.exact(25))
    }
  }

}
