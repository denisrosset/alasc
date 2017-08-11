package net.alasc.tests
package finite

import spire.laws.{LatticeLaws, LatticePartialOrderLaws}

import org.scalacheck.{Arbitrary, Gen}

import net.alasc.domains.Partition
import net.alasc.finite.{Grp, GrpGroup}
import net.alasc.laws._
import net.alasc.perms.Perm
import net.alasc.perms.default._

class GrpPermSuite extends AlascSuite {

  import Permutations.arbPerm
  import Grps.arbGrp

  val build = implicitly[GrpGroup[Perm]]

  checkAll("Grp[Perm] with permutation action", GrpLaws[Perm].grpPermutationAction)

  checkAll("Group lattice laws", LatticePartialOrderLaws[Grp[Perm]].boundedBelowLatticePartialOrder)

}
