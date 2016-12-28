package net.alasc.tests
package perms

import spire.algebra.Eq
import spire.laws.{LatticePartialOrderLaws, Perm => _}
import spire.math.SafeLong

import org.scalacheck.Arbitrary

import net.alasc.algebra._
import net.alasc.domains.Domain
import net.alasc.finite.{FaithfulPermutationActionBuilder, Grp, Rep}
import net.alasc.laws._
import net.alasc.perms._
import net.alasc.rep.FaithfulPermRepBuilder

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
/* TODO
  {
    import net.alasc.perms.default._
    import Grps.arbGrp
    import net.alasc.finite.Rep.algebra._

    val domain1 = Domain(3)
    val domain2 = Domain(3)
    val coveringDomain = (Perm(0,1,2), Perm(0,1,2))

    val rep: FaithfulPermRep[(Perm, Perm), SafeLong] = FaithfulPermRepBuilder[(Perm, Perm)].build[SafeLong](Seq(coveringDomain))

    type R = Rep.Of[(Perm, Perm), rep.type]

    implicit val permTupleArbitrary: Arbitrary[R] =
      Arbitrary(for {
        g1 <- Permutations.forDomain[Perm](domain1)
        g2 <- Permutations.forDomain[Perm](domain2)
      } yield Rep.Of((g1, g2), rep))

    checkAll("Group laws", GrpLaws[R].grp)
    checkAll("Group lattice laws", LatticePartialOrderLaws[Grp[R]].boundedBelowLatticePartialOrder)
  }*/

}
