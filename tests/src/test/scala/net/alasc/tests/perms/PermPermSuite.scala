package net.alasc.tests
package perms

import spire.laws.{LatticePartialOrderLaws, Perm => _}

import org.scalacheck.Arbitrary

import net.alasc.algebra._
import net.alasc.finite.Grp
import net.alasc.laws._
import net.alasc.perms._
import net.alasc.std.product._

class PermPermSuite extends AlascSuite {

  import Permutations.arbDom

  {
    implicit val permTupleArbitrary: Arbitrary[(Perm, Perm)] =
      Arbitrary(for {
        g1 <- Permutations.forSize[Perm](16)
        g2 <- Permutations.forSize[Perm](5)
      } yield (g1, g2))

    implicit val permPermAction: FaithfulPermutationAction[(Perm, Perm)] =
      implicitly[FaithfulPermRepBuilder[(Perm, Perm)]].build(Seq((Perm(0, 15), Perm(0, 4)))).permutationAction

    checkAll("(Perm, Perm)", PermutationActionLaws[(Perm, Perm)].faithfulPermutationAction)
  }

  {
    import net.alasc.perms.default._

    implicit val permTupleArbitrary: Arbitrary[(Perm, Perm)] =
      Arbitrary(for {
        g1 <- Permutations.forSize[Perm](3)
        g2 <- Permutations.forSize[Perm](3)
      } yield (g1, g2))

    import Grps.arbGrp

    checkAll("Group laws", GrpLaws[(Perm, Perm)].grp)
    checkAll("Group lattice laws", LatticePartialOrderLaws[Grp[(Perm, Perm)]].boundedBelowLatticePartialOrder)
  }

}
