package net.alasc.tests
package lexico

import spire.algebra.Order

import net.alasc.tests.AlascSuite
import spire.syntax.cfor._

import org.scalacheck.{Arbitrary, Gen}

class LexSetIntOrderSuite extends AlascSuite {

  implicit def arbSetInt: Arbitrary[Set[Int]] = Arbitrary {
    for {
      n <- Gen.choose(0, 100)
      set <- Gen.containerOfN[Set, Int](n, Gen.choose(0, 10000))
      conv <- Gen.oneOf[Set[Int] => Set[Int]](
        (st: Set[Int]) => collection.immutable.BitSet.empty ++ st,
        (st: Set[Int]) => collection.immutable.SortedSet.empty[Int] ++ st,
        (st: Set[Int]) => collection.immutable.HashSet.empty[Int] ++ st,
        (st: Set[Int]) => st
      )
    } yield conv(set)
  }

  import net.alasc.lexico.lexSetIntOrder._
  import net.alasc.lexico.lexSeqOrder._

  test("Explicit examples for set lexicographic order") {
      val data = Vector(Set.empty[Int], Set(0), Set(0,1), Set(1))
      cforRange(0 until data.length) { i =>
        Order[Set[Int]].compare(data(i), data(i)) shouldBe 0
      }
      cforRange(1 until data.length) { i =>
        val (before, after) = data.splitAt(i)
        for(b <- before; a <- after) {
          Order[Set[Int]].compare(b, a) should be < 0
          Order[Set[Int]].compare(a, b) should be > 0
        }
      }
    }

  checkAll("Set[Int]", spire.laws.OrderLaws[Set[Int]].order)

  test("Lexicographic set order is compatible with lexicographic seq order") {
    forAll { (x: Set[Int], y: Set[Int]) =>
      val xseq = x.toSeq.sorted
      val yseq = y.toSeq.sorted
      Order[Seq[Int]].compare(xseq, yseq).signum shouldBe Order[Set[Int]].compare(x, y).signum
    }
  }

}
