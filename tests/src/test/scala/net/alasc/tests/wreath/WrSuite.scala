package net.alasc.tests.wreath

import spire.laws.{Perm => _}
import spire.math.SafeLong

import net.alasc.domains.Domain
import net.alasc.finite.Grp
import net.alasc.laws._
import net.alasc.perms.Perm
import net.alasc.tests.AlascSuite
import net.alasc.wreath.Wr

class WrSuite extends AlascSuite {

  import Permutations.arbPerm
  import Wrs.arbWr

  implicit val wrNoShrink = noShrink[Wr[Perm]]
/*
  val domain = Domain(100)

  nestedCheckAll[WrSize]("Wr[Perm,Perm]", WrSize(1, 1)) { implicit wrSize =>
    implicit def action = wrSize.representation[SafeLong].permutationAction
    PermutationActionLaws[Wr[Perm]](domain).faithfulPermutationAction
  }*/

  import net.alasc.perms.default._

  import Grps.arbGrp
/*
  nestedCheckAll[WrSize]("Wr[Perm,Perm]", WrSize(1, 1)) { implicit wrSize =>
    GrpLaws[Wr[Perm]].grpWithoutHashCodeEquals
  }*/

  test("Bug discovered by random testing") {
    val arg1 = Grp(Wr[Perm]()())
    val arg2 = Wr(0 -> Perm(0,1,2), 1 -> Perm(0,2,1), 2 -> Perm(0,1,2), 3 -> Perm(0,1), 4 -> Perm(0,2))(0,1,2)
    val arg3 = Wr[Perm]()()
    val grp = arg1.conjugatedBy(arg2)
    grp.contains(arg2.inverse |+| arg3 |+| arg2) shouldBe true
  }

}
