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
    val domain = Domain(30)
    implicit val permTupleArbitrary: Arbitrary[(Perm, Perm)] =
      Arbitrary(for {
        g1 <- Permutations.forSize[Perm](16)
        g2 <- Permutations.forSize[Perm](5)
      } yield (g1, g2))

    implicit val permPermAction: PermutationAction[(Perm, Perm)] =
      implicitly[FaithfulPermRepBuilder[(Perm, Perm)]].build[SafeLong](Seq((Perm(0, 15), Perm(0, 4)))).permutationAction

    checkAll("(Perm, Perm)", PermutationActionLaws[(Perm, Perm)](domain).faithfulPermutationAction)
  }

  {
    import net.alasc.perms.default._

    val rep: FaithfulPermRep[(Perm, Perm), SafeLong] =
      implicitly[FaithfulPermRepBuilder[(Perm, Perm)]].build[SafeLong](Seq((Perm(0,1,2), Perm(0,1,2))))

    import Grps.arbGrp

    import net.alasc.finite.Rep.algebra._

    type R = Rep.Of[(Perm, Perm), rep.type]
    implicitly[Eq[(Perm, Perm)]]
    Rep.algebra.permutation[(Perm, Perm), rep.type]
    implicitly[Permutation[R]]
    implicit val permTupleArbitrary: Arbitrary[R] =
      Arbitrary(for {
        g1 <- Permutations.forSize[Perm](3)
        g2 <- Permutations.forSize[Perm](3)
      } yield Rep.Of((g1, g2), rep))

    checkAll("Group laws", GrpLaws[R].grp)
    checkAll("Group lattice laws", LatticePartialOrderLaws[Grp[R]].boundedBelowLatticePartialOrder)
  }

}
