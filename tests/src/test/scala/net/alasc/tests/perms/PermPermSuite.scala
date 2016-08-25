package net.alasc.tests
package perms

import spire.algebra.Eq
import spire.laws.{LatticePartialOrderLaws, Perm => _}
import spire.math.SafeLong

import org.scalacheck.Arbitrary

import net.alasc.algebra._
import net.alasc.domains.Domain
import net.alasc.finite.{Grp, Rep}
import net.alasc.laws._
import net.alasc.perms._

class PermPermSuite extends AlascSuite {

  import Doms.arbDomInDomain

  {
    val domain = Domain(21)
    val leftDomain = Domain(16)
    val rightDomain = Domain(5)
    implicit val permTupleArbitrary: Arbitrary[(Perm, Perm)] =
      Arbitrary(for {
        g1 <- Permutations.forDomain[Perm](leftDomain)
        g2 <- Permutations.forDomain[Perm](rightDomain)
      } yield (g1, g2))

    val coveringDomain = (Perm(0, leftDomain.size -1), Perm(0, rightDomain.size - 1))
    implicit val permPermAction: PermutationAction[(Perm, Perm)] =
      FaithfulPermRepBuilder[(Perm, Perm)].build[SafeLong](Seq(coveringDomain)).permutationAction

    checkAll("(Perm, Perm)", PermutationActionLaws[(Perm, Perm)](domain).faithfulPermutationAction)
  }

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
  }

}
