package net.alasc.tests
package lexico

import spire.algebra.Order

import net.alasc.tests.AlascSuite

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

  test("Discovered bug") {
    val x = collection.immutable.BitSet(221, 377, 512, 600, 690, 1282, 1567, 1723, 2278, 2548, 2595, 2695, 2888, 3454, 3532, 3605, 3622, 3669, 3719, 3828, 4239, 4248, 4406, 4569, 4608, 5148, 5180, 5298, 5315, 5717, 6029, 6040, 6115, 6189, 6231, 6460, 6505, 6705, 6861, 6948, 6974, 7174, 7273, 7527, 7657, 8032, 8069, 8251, 8326, 8421, 8598, 8805, 9594, 9844)
    val y = collection.immutable.BitSet(79, 376, 430, 563, 1293, 1532, 1601, 1779, 2509, 2982, 3007, 3070, 3293, 3353, 3490, 3653, 3699, 3973, 4454, 4608, 5106, 5529, 5580, 5633, 5902, 6037, 6052, 6486, 6641, 6885, 7049, 7251, 7331, 7659, 7773, 7887, 7937, 7953, 8044, 8111, 8549, 8551, 9005, 9228, 9468, 9540, 9724, 9937, 9995)
    val xseq = x.toSeq.sorted
    val yseq = y.toSeq.sorted
    Order[Seq[Int]].compare(xseq, yseq).signum shouldBe Order[Set[Int]].compare(x, y).signum
  }

}
