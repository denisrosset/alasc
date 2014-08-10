package net.alasc.math

import net.alasc.algebra._

import org.scalacheck._
import org.scalacheck.Arbitrary._
import org.scalatest._
import prop._
import spire.syntax.group._
import spire.syntax.groupAction._
import spire.syntax.eq._
import net.alasc.syntax.permutation._

class CyclesCheck extends PermutationCheck[Cycles] with PermutationGenerators[Cycles] {
  implicit def algebra = Cycles.Algebra

  property("x.to[Perm].to[Cycles] === x") {
    forAll { (x: Cycles) =>
      x.to[Perm].to[Cycles] shouldEqv x
    }
  }

  property("x.support === x.to[Perm].support") {
    forAll { (x: Cycles) =>
      x.support shouldBe x.to[Perm].support
    }
  }

  property("x.supportMin === x.to[Perm].supportMin") {
    forAll { (x: Cycles) =>
      x.support shouldBe x.to[Perm].support
    }
  }

  property("x.supportMax === x.to[Perm].supportMax") {
    forAll { (x: Cycles) =>
      x.support shouldBe x.to[Perm].support
    }
  }
}
