package net.alasc.math

import org.scalacheck._
import org.scalacheck.Arbitrary._
import org.scalatest._
import prop._

import spire.algebra.Eq
import spire.syntax.group._
import spire.syntax.groupAction._
import spire.syntax.eq._

import net.alasc.algebra._
import net.alasc.syntax.permutationAction._


trait PermutationGenerators[P] {
  implicit def algebra: Permutation[P]
  implicit def maximumSize: Int = algebra.supportMaxElement.min(100000)
  implicit def arbitrary: Arbitrary[P] =
    Arbitrary {
      import spire.std.int._
      def genFromSeq(sz: Int) = for {
        n <- Gen.choose(sz/2 + 1, sz + 1)
        seq <- Gen.containerOfN[Seq, Int](maximumSize.min(n), Gen.choose(1, 10000))
      } yield algebra.sorting(seq)
      Gen.sized(sz => genFromSeq(sz))
    }
}

trait PermutationCheck[P]
    extends PropSpec with Matchers with EqMatchers with GeneratorDrivenPropertyChecks with NonImplicitAssertions {
  implicit def algebra: Permutation[P]
  implicit def maximumSize: Int
  implicit def arbitrary: Arbitrary[P]

  property("x |+| identity === x") {
    forAll { (x: P) =>
      (algebra.id |+| x) shouldEqv x
    }
  }
  property("x.inverse |+| x === identity") {
    forAll { (x: P) =>
      (x.inverse |+| x) shouldEqv algebra.id
    }
  }
  property("x.inverse.inverse === x") {
    forAll { (x: P) =>
      x.inverse.inverse shouldEqv x
    }
  }
  property("(x |+| y).inverse === (y.inverse |+| x.inverse)") {
    forAll { (x: P, y: P) =>
      (x |+| y).inverse shouldEqv (y.inverse |+| x.inverse)
    }
  }
  property("k <|+| (x |+| y) == (k <|+| x) <|+| y") {
    forAll { (kk: Int, x: P, y: P) =>
      val k = (kk & 0x7FFFFFFF) % (maximumSize * 2)
      (k <|+| (x |+| y)) shouldBe ((k <|+| x) <|+| y)
    }
  }
  property("(y |+| x) |+|> k == (y |+|> (x |+|> k))") {
    forAll { (kk: Int, x: P, y: P) =>
      val k = (kk & 0x7FFFFFFF) % (maximumSize * 2)
      ((y |+| x) |+|> k) shouldBe (y |+|> (x |+|> k))
    }
  }
  property("k <|+| x == x.inverse |+|> k") {
    forAll { (kk: Int, x: P) =>
      val k = (kk & 0x7FFFFFFF) % (maximumSize * 2)
      (k <|+| x) shouldBe (x.inverse |+|> k)
    }
  }
}
