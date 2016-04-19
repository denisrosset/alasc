package net.alasc.tests
package finite

import spire.laws.LatticePartialOrderLaws

import net.alasc.finite.Grp
import net.alasc.laws.{Grps, PermGrpLaws, Permutations}
import net.alasc.perms.Perm
import net.alasc.perms.default._

class GrpPermSuite extends AlascSuite {

  import Permutations.{arbDom, arbPermutation}
  import Grps.arbGrp

  checkAll("Group laws", PermGrpLaws[Perm].permGrp)
  checkAll("Group lattice laws", LatticePartialOrderLaws[Grp[Perm]].boundedBelowLatticePartialOrder)

}
