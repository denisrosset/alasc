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
  implicit def permutation: Permutation[P]
  implicit def maximumSize: Int = permutation.supportMaxElement.min(100000)

  def genP(n: Int): Gen[P] = for {
    seq <- Gen.containerOfN[Seq, Int](maximumSize.min(n) - 1, Gen.choose(1, 10000))
    k <- Gen.choose(0, n - 2)
    perm = permutation.sorting(seq)
  } yield perm |+| permutation.from(Cycles(k, n - 1))

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

trait FiniteGroupCheck[G]
    extends PropSpec with Matchers with EqMatchers with GeneratorDrivenPropertyChecks with NonImplicitAssertions {
  implicit def finiteGroup: FiniteGroup[G]
  implicit def arbitrary: Arbitrary[G]
  property("x |+| identity === x") {
    forAll { (x: G) =>
      (finiteGroup.id |+| x) shouldEqv x
    }
  }
  property("x.inverse |+| x === identity") {
    forAll { (x: G) =>
      (x.inverse |+| x) shouldEqv finiteGroup.id
    }
  }
  property("(x.inverse |+| x).isId is true") {
    forAll { (x: G) =>
      (x.inverse |+| x).isId shouldBe true
    }
  }
  property("x.inverse.inverse === x") {
    forAll { (x: G) =>
      x.inverse.inverse shouldEqv x
    }
  }
  property("(x |+| y).inverse === (y.inverse |+| x.inverse)") {
    forAll { (x: G, y: G) =>
      (x |+| y).inverse shouldEqv (y.inverse |+| x.inverse)
    }
  }
  property("x y == x iff y.isIdentity") {
    forAll { (x: G, y: G) =>
      ((x |+| y) =!= x) || y.isId shouldBe true
    }
  }
}

trait PermutationActionCheck[G] extends FiniteGroupCheck[G] {
  implicit def action: PermutationAction[G]
  implicit def maximumSize: Int

  property("k <|+| (x |+| y) == (k <|+| x) <|+| y") {
    forAll { (kk: Int, x: G, y: G) =>
      val k = (kk & 0x7FFFFFFF) % (maximumSize * 2)
      (k <|+| (x |+| y)) shouldBe ((k <|+| x) <|+| y)
    }
  }
  property("(y |+| x) |+|> k == (y |+|> (x |+|> k))") {
    forAll { (kk: Int, x: G, y: G) =>
      val k = (kk & 0x7FFFFFFF) % (maximumSize * 2)
      ((y |+| x) |+|> k) shouldBe (y |+|> (x |+|> k))
    }
  }
  property("k <|+| x == x.inverse |+|> k") {
    forAll { (kk: Int, x: G) =>
      val k = (kk & 0x7FFFFFFF) % (maximumSize * 2)
        (k <|+| x) shouldBe (x.inverse |+|> k)
    }
  }
  property("x.support.isEmpty == x.isId") {
    forAll { x: G =>
      x.support.isEmpty shouldBe x.isId
    }
  }
  property("x.support.min == x.supportMin") {
    forAll { x: G =>
      whenever (!x.isId) {
        x.support.min == x.supportMin.get
      }
    }
  }
  property("x.support.max == x.supportMax") {
    forAll { x: G =>
      whenever (!x.isId) {
        x.support.max == x.supportMax.get
      }
    }
  }
}

trait PermutationCheck[P] extends PermutationActionCheck[P] {
  def permutation: Permutation[P]
  implicit def finiteGroup = permutation
  implicit def action = permutation
}
