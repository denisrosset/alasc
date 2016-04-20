package net.alasc.tests.wreath

import spire.laws.{LatticePartialOrderLaws, Perm => _}

import org.scalacheck.Arbitrary

import net.alasc.finite.Grp
import net.alasc.laws._
import net.alasc.perms.Perm
import net.alasc.tests.AlascSuite
import net.alasc.wreath.Wr

class WrSuite extends AlascSuite {

  import net.alasc.laws.Permutations.arbDom
  import net.alasc.laws.Permutations.arbPermutation
  import net.alasc.laws.Wrs.arbWr

  implicit val wrNoShrink = noShrink[Wr[Perm, Perm]]

  nestedCheckAll[WrSize]("Wr[Perm,Perm]", WrSize(1, 1)) { implicit wrSize =>
    implicit def action = wrSize.representation[Perm, Perm].permutationAction
    PermutationActionLaws[Wr[Perm, Perm]].faithfulPermutationAction
  }

  import net.alasc.perms.default._

  import Grps.arbGrp

  nestedCheckAll[WrSize]("Wr[Perm,Perm]", WrSize(1, 1)) { implicit wrSize =>
    GrpLaws[Wr[Perm, Perm]].grpWithoutHashCodeEquals
  }

  test("Bug discovered by random testing") {
    val arg1 = Grp(Wr(Seq(Perm.id), Perm.id))
    val arg2 = Wr(Vector(Perm(0,1,2), Perm(0,2,1), Perm(0,1,2), Perm(0,1), Perm(0,2)), Perm(0,1,2))
    val arg3 = Wr(List[Perm](), Perm.id)
    val grp = arg1.conjugatedBy(arg2)
    grp.contains(arg2.inverse |+| arg3 |+| arg2) shouldBe true
  }

}
