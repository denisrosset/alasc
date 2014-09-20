package net.alasc.algebra

import scala.util.Random

import org.scalacheck._
import org.scalacheck.Arbitrary._
import org.scalatest._
import prop._

import spire.syntax.partialOrder._
import spire.std.boolean._

import net.alasc.syntax.lattice._

trait LatticeCheck[L] extends PropSpec with Matchers with EqMatchers with GeneratorDrivenPropertyChecks with NonImplicitAssertions {
  implicit def lattice: Lattice[L]
  implicit def arbitraryLatticeElement: Arbitrary[L]

  property("Join is commutative") {
    forAll { (a: L, b: L) => (a join b) shouldEqv (b join a) }
  }

  property("Meet is commutative") {
    forAll { (a: L, b: L) => (a meet b) shouldEqv (b meet a) }
  }

  property("Join is associative") {
    forAll { (a: L, b: L, c: L) => (a join (b join c)) shouldEqv ((a join b) join c) }
  }

  property("Meet is associative") {
    forAll { (a: L, b: L, c: L) => (a meet (b meet c)) shouldEqv ((a meet b) meet c) }
  }

  property("Absorption: (a join (a meet b)) = a") {
    forAll { (a: L, b: L) => (a join (a meet b)) shouldEqv a }
  }

  property("Absorption: (a meet (a join b)) = a") {
    forAll { (a: L, b: L) => (a meet (a join b)) shouldEqv a }
  }

  property("Join is idempotent") {
    forAll { (a: L) => (a join a) shouldEqv a }
  }

  property("Meet is idempotent") {
    forAll { (a: L) => (a meet a) shouldEqv a }
  }

  property("Partial order compatible with meet") {
    forAll { (a: L, b: L) =>
      (a <= b) shouldEqv (a === (a meet b))
    }
  }

  property("Partial order compatible with join") {
    forAll { (a: L, b: L) =>
      (a <= b) shouldEqv (b === (a join b))
    }
  }
}

trait BoundedBelowLatticeCheck[L] extends LatticeCheck[L] {
  implicit def lattice: BoundedBelowLattice[L]

  property("(a meet 0) === 0") {
    forAll { (a: L) =>
      (a meet lattice.zero) shouldEqv lattice.zero
    }
  }

  property("0 <= a") {
    forAll { (a: L) =>
      (lattice.zero <= a) shouldEqv true
    }
  }
}

trait BoundedLatticeCheck[L] extends BoundedBelowLatticeCheck[L] {
  implicit def lattice: BoundedLattice[L]

  property("(a join 1) === 1") {
    forAll { (a: L) =>
      (a join lattice.one) shouldEqv lattice.one
    }
  }

  property("a <= 1") {
    forAll { (a: L) =>
      (a <= lattice.one) shouldEqv true
    }
  }
}
