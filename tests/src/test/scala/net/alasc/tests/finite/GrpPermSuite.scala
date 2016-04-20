package net.alasc.tests
package finite

import spire.laws.LatticePartialOrderLaws

import net.alasc.algebra.PermutationBuilder
import net.alasc.finite.Grp
import net.alasc.laws.{Grps, PermGrpLaws, Permutations}
import net.alasc.perms.{Perm, PermGrpBuilder}
import net.alasc.perms.default._

class GrpPermSuite extends AlascSuite {

  import Permutations.{arbDom, arbPermutation}
  import Grps.arbGrp

  implicitly[PermutationBuilder[Perm]]
  val build = implicitly[PermGrpBuilder[Perm]]
  val pl: PermGrpLaws[Perm] = PermGrpLaws.apply[Perm]
  checkAll("Group laws", pl.permGrp(build))
  checkAll("Group lattice laws", LatticePartialOrderLaws[Grp[Perm]].boundedBelowLatticePartialOrder)

}
