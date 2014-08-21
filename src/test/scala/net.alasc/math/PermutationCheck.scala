package net.alasc.math

import org.scalacheck._
import org.scalacheck.Arbitrary._
import org.scalatest._
import prop._

import spire.algebra.Eq
import spire.syntax.group._
import spire.syntax.groupAction._
import spire.syntax.eq._
import spire.std.int._

import net.alasc.algebra._
import net.alasc.syntax.permutationAction._


trait PermutationGenerators[P] {
  implicit def algebra: Permutation[P]
  implicit def maximumSize: Int = algebra.supportMaxElement.min(100000)

  def genP(n: Int): Gen[P] = for {
    seq <- Gen.containerOfN[Seq, Int](maximumSize.min(n) - 1, Gen.choose(1, 10000))
    k <- Gen.choose(0, n - 2)
    perm = algebra.sorting(seq)
  } yield perm |+| algebra.from(Cycles(k, n - 1))

  implicit def arbitrary: Arbitrary[P] =
    Arbitrary {
      import spire.std.int._
      def genFromSeq(sz: Int) = for {
        n <- Gen.choose(sz/2 + 2, sz + 2)
        p <- genP(n)
      } yield p
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
  property("x y == x iff y.isIdentity") {
    forAll { (x: P, y: P) =>
      ((x |+| y) =!= x) || y.isId shouldBe true
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
