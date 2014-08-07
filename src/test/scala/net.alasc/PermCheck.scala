package net.alasc.math
package perm


import org.scalacheck._
import org.scalacheck.Arbitrary._
import org.scalatest._
import prop._
import spire.syntax.group._
import spire.syntax.groupAction._
import spire.syntax.eq._
import net.alasc.algebra._
import net.alasc.syntax.permutation._

class PermCheck extends PermutationCheck[Perm] with PermutationGenerators[Perm] {
  implicit def algebra = Perm.Algebra

  property("x === fromImages(x.images)") {
    forAll { (x: Perm) =>
      Perm.Algebra.fromImages((0 to x.supportMax).map(_ <|+| x)) shouldEqv x
    }
  }

  property("x |+| y === (x.to[Cycles] |+| y.to[Cycles]).to[Perm]") {
    forAll { (x: Perm, y: Perm) =>
      (x |+| y) shouldEqv (x.to[Cycles] |+| y.to[Cycles]).to[Perm]
    }
  }
}
