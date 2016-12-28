package net.alasc.tests
package perms

import spire.math.SafeLong

import net.alasc.finite.{Grp, GrpGroup, GrpPermutationAction}
import net.alasc.named.Mathieu
import net.alasc.perms.Perm

class MathieuSuite(implicit gg: GrpGroup[Perm], ga: GrpPermutationAction[Perm]) extends AlascSuite {

  test("Mathieu group constructions have correct order") {
    Mathieu.generatorsAndOrders.foreach {
      case (degree, (generators, order)) =>
        val grp = Grp.fromGeneratorsAndOrder(generators, order)
        grp.order shouldBe order
    }
  }

  test("Mathieu group 22 one-point stabilizers have order 20160") {
    val grp = Mathieu(22)
    forAll(Table("k", 1 to 22:_*)) { k =>
      grp.stabilizer(k).order should ===(SafeLong(20160))
    }
  }

  test("Mathieu group 11 one-point stabilizers have order 720") {
    val grp = Mathieu(11)
    forAll(Table("k", 1 to 11: _*)) { k =>
      grp.stabilizer(k).order should ===(SafeLong(720))
    }
  }

  test("Mathieu group 11 stabilizer of {2,9} has order 144") {
    Mathieu(11).setwiseStabilizer(2, 9).order should === (SafeLong(144))
  }

}

class MathieuSuiteDeterministic extends MathieuSuite()(PermSuite.deterministic, PermSuite.deterministic)

class MathieuSuiteRandomized extends MathieuSuite()(PermSuite.randomized, PermSuite.randomized)