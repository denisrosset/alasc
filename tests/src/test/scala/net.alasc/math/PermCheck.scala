package net.alasc.math

import org.scalacheck._
import org.scalacheck.Arbitrary._
import org.scalatest._
import prop._
import spire.algebra.Eq
import spire.syntax.group._
import spire.syntax.signed._
import spire.syntax.groupAction._
import spire.syntax.eq._
import net.alasc.algebra._
import net.alasc.syntax.permutationAction._

class PermHashCheck extends HashCheck[Perm] with PermutationGenerators[Perm] {
  implicit def permutation = Perm.Algebra
  implicit def arb = Arbitrary { genP(30) }
  def clone(p: Perm) = p.to[Cycles].to[Perm]
  implicit def eq: Eq[Perm] = Perm.Algebra
}

class PermCheck extends PermutationCheck[Perm] with PermutationGenerators[Perm] {
  implicit def permutation = Perm.Algebra

  property("(x.signum * y.signum) == (x |+| y).signum") {
    forAll { (x: Perm, y: Perm) => (x.signum * y.signum) shouldBe (x |+| y).signum }
  }
  property("x === fromImages(x.images)") {
    forAll { (x: Perm) =>
      Perm.Algebra.fromImages((0 to x.supportMax.getOrElse(16)).map(_ <|+| x)) shouldEqv x
    }
  }

  property("x |+| y === (x.to[Cycles] |+| y.to[Cycles]).to[Perm]") {
    forAll { (x: Perm, y: Perm) =>
      (x |+| y) shouldEqv (x.to[Cycles] |+| y.to[Cycles]).to[Perm]
    }
  }

  property("x.inverse.to[Cycles] === x.to[Cycles].inverse") {
    forAll { (x: Perm) =>
      x.support shouldBe x.to[Cycles].support
    }
  }

  property("x === x.to[Cycles].support") {
    forAll { (x: Perm, k: Int) =>
      x.support shouldBe x.to[Cycles].support
    }
  }

  property("x.support === x.to[Cycles].support") {
    forAll { (x: Perm) =>
      x.support shouldBe x.to[Cycles].support
    }
  }

  property("x.supportMin === x.to[Cycles].supportMin") {
    forAll { (x: Perm) =>
      x.support shouldBe x.to[Cycles].support
    }
  }

  property("x.supportMax === x.to[Cycles].supportMax") {
    forAll { (x: Perm) =>
      x.support shouldBe x.to[Cycles].support
    }
  }
}
