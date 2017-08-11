package net.alasc.tests
package perms

import spire.algebra.Eq
import spire.laws.{LatticePartialOrderLaws, Perm => _}
import spire.math.SafeLong

import org.scalacheck.Arbitrary

import net.alasc.algebra._
import net.alasc.finite.{FaithfulPermutationActionBuilder, Grp, Rep}
import net.alasc.laws._
import net.alasc.perms._

class PermPermSuite extends AlascSuite {

  {
    val size = 21
    val leftSize = 16
    val rightSize = 5
    implicit val permTupleArbitrary: Arbitrary[(Perm, Perm)] =
      Arbitrary(for {
        g1 <- Permutations.permForSize(leftSize)
        g2 <- Permutations.permForSize(rightSize)
      } yield (g1, g2))

    val coveringDomain = (Perm(0, leftSize -1), Perm(0, rightSize - 1))
    implicit val permPermAction: PermutationAction[(Perm, Perm)] = FaithfulPermutationActionBuilder[(Perm, Perm)].apply(Seq(coveringDomain))

    checkAll("(Perm, Perm)", PermutationActionLaws[(Perm, Perm)].faithfulPermutationAction)

  }
  {
    import Grps.arbGrp
    import net.alasc.perms.default._
    val size = 6
    val leftSize = 3
    val rightSize = 3

    implicit val permTupleArbitrary: Arbitrary[(Perm, Perm)] =
      Arbitrary(for {
        g1 <- Permutations.permForSize(leftSize)
        g2 <- Permutations.permForSize(rightSize)
      } yield (g1, g2))

    checkAll("Group laws", GrpLaws[(Perm, Perm)].grp)
    checkAll("Group lattice laws", LatticePartialOrderLaws[Grp[(Perm, Perm)]].boundedBelowLatticePartialOrder)

  }

}
