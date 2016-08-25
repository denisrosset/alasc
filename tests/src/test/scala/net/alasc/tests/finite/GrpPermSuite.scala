package net.alasc.tests
package finite

import spire.laws.{LatticeLaws, LatticePartialOrderLaws}

import org.scalacheck.{Arbitrary, Gen}

import net.alasc.domains.{Domain, Partition}
import net.alasc.finite.Grp
import net.alasc.laws._
import net.alasc.perms.{Perm, PermGrpBuilder}
import net.alasc.perms.default._

class GrpPermSuite extends AlascSuite {

  import Permutations.arbPermutation
  import Grps.arbGrp

  val build = implicitly[PermGrpBuilder[Perm]]

  implicit def arbDomain: Arbitrary[Domain] = Arbitrary( Gen.choose(0, 32).map(Domain(_)) )

  nestedCheckAll[Domain]("Grp[Perm] laws", Domain(0)) { d =>
    val domain: Domain = d
    import Doms.arbDomInDomain
    PermGrpLaws[Perm](domain).permGrp
  }

//  checkAll("Grp[Perm] laws", PermGrpLaws[Perm]()

  checkAll("Group lattice laws", LatticePartialOrderLaws[Grp[Perm]].boundedBelowLatticePartialOrder)

}
