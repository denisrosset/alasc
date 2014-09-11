package net.alasc.math

import org.scalacheck._
import org.scalacheck.Arbitrary._
import org.scalatest._
import prop._
import spire.syntax.eq._
import net.alasc.algebra._

class GrpLatticeCheck extends BoundedBelowLatticeCheck[Grp[Perm]] with PermutationGenerators[Perm] {
  implicit def algorithms = Grp.defaultAlgorithms[Perm]
  implicit val lattice = new GrpLattice[Perm]
  implicit def permutation = Perm.Algebra
  implicit def arbitraryLatticeElement = Arbitrary {
    for {
      gen1 <- genP(8)
      gen2 <- genP(8)
      gen3 <- genP(8)
    } yield Grp(gen1, gen2, gen3)
  }
}
