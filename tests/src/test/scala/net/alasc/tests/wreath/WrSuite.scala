package net.alasc.tests.wreath

import spire.laws.{Perm => _}
import spire.math.SafeLong

import org.scalacheck.Arbitrary

import net.alasc.domains.Domain
import net.alasc.finite.Grp
import net.alasc.laws._
import net.alasc.perms.Perm
import net.alasc.tests.AlascSuite
import net.alasc.wreath.{Wr, WrFaithfulPermutationAction}
import net.alasc.perms.default._

class WrSuite extends AlascSuite {

  import Permutations.arbPerm


  implicit val wrNoShrink = noShrink[Wr[Perm]]

  {
    implicit val arbWr: Arbitrary[Wr[Perm]] = Arbitrary(Wrs.forSize(5, 5))
    implicit val action = new WrFaithfulPermutationAction[Perm](5, 5)
    checkAll("PermutationAction[Wr[Perm]]", PermutationActionLaws[Wr[Perm]].faithfulPermutationAction)
  }

  {
    val genWr = Wrs.forSize(3, 3)
    implicit val arbWr: Arbitrary[Wr[Perm]] = Arbitrary(genWr)
    implicit val arbGrp = Arbitrary(Grps.fromElements(genWr))
    checkAll("Grp[Wr[Perm]]", GrpLaws[Wr[Perm]].grpPermutationAction)
  }

  test("Bug discovered by random testing") {
    val arg1 = Grp(Wr[Perm]()())
    val arg2 = Wr(0 -> Perm(0,1,2), 1 -> Perm(0,2,1), 2 -> Perm(0,1,2), 3 -> Perm(0,1), 4 -> Perm(0,2))(0,1,2)
    val arg3 = Wr[Perm]()()
    val grp = arg1.conjugatedBy(arg2)
    grp.contains(arg2.inverse |+| arg3 |+| arg2) shouldBe true
  }

}
