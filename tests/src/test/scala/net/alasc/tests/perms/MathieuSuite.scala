package net.alasc.tests
package perms

import spire.math.SafeLong

import net.alasc.named.Mathieu
import net.alasc.perms.{Perm, PermGrpBuilder}
import net.alasc.tests.bsgs.BSGSSuite

class MathieuSuite(implicit builder: PermGrpBuilder[Perm]) extends AlascSuite {

  def allPointStabilizersHaveOrder(first: Int, degree: Int, stabilizerOrder: SafeLong): Boolean = {
    val grp = Mathieu[Perm](degree)
    val domain = (first until first + degree)
    domain.forall ( k => grp.stabilizer(k).order === stabilizerOrder )
  }

  test("Mathieu group constructions have correct order") {
    Mathieu.generatorsAndOrders.foreach {
      case (degree, (generators, order)) =>
        val grp = builder.fromGeneratorsAndOrder(generators, order)
        grp.order shouldBe order
    }
  }


    test("Mathieu group 22 one-point stabilizers have order 20160") {
      allPointStabilizersHaveOrder(1, 22, 20160) shouldBe true
    }

    test("Mathieu group 11 one-point stabilizers have order 720") {
      allPointStabilizersHaveOrder(1, 11, 720)
    }

    test("Mathieu group 11 stabilizer of {2,9} has order 144") {
      Mathieu[Perm](11).setwiseStabilizer(2, 9).order should === (SafeLong(144))
    }

  }

  class MathieuSuiteDeterministic extends MathieuSuite()(BSGSSuite.deterministic)

  class MathieuSuiteRandomized extends MathieuSuite()(BSGSSuite.randomized)