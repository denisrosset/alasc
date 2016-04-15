package net.alasc.perms

import org.scalatest.{FunSuite, NonImplicitAssertions, Matchers}

import net.alasc.named.Mathieu
import net.alasc.prep._

class MathieuSuite extends FunSuite with NonImplicitAssertions with Matchers {

  test("Mathieu group constructions have correct order") {
    def test()(implicit builder: PGrpBuilder[Perm]): Unit = {
      Mathieu.generatorsAndOrders.foreach {
        case (degree, (generators, order)) =>
          val grp = builder.fromGeneratorsAndOrder(generators, order)
          grp.order shouldBe order
      }
    }
    {
      import PGrp.deterministic._
      test()
    }
    {
      import PGrp.default._ // TODO: use fixed seed
      test()
    }
  }

  def testPointStabilizers(first: Int, degree: Int, stabilizerOrder: BigInt)(implicit builder: PGrpBuilder[Perm]): Unit = {
    val grp = Mathieu[Perm](degree)
    val domain = (first until first + degree)
    domain.foreach ( k => grp.stabilizer(k).order shouldBe stabilizerOrder )
  }

  test("Mathieu group 22 one-point stabilizers have order 20160 / deterministic") {
    import PGrp.deterministic._
    testPointStabilizers(1, 22, 20160)
  }

  test("Mathieu group 22 one-point stabilizers have order 20160 / randomized") {
    import PGrp.default._ // TODO: use fixed seed
    testPointStabilizers(1, 22, 20160)
  }

  test("Mathieu group 11 one-point stabilizers have order 720 / deterministic") {
    import PGrp.deterministic._
    testPointStabilizers(1, 11, 720)
  }

  test("Mathieu group 11 one-point stabilizers have order 720 / randomized") {
    import PGrp.default._ // TODO: use fixed seed
    testPointStabilizers(1, 11, 720)
  }

  test("Mathieu group 11 stabilizer of {2,9} has order 144") {
    {
      import PGrp.deterministic._
      Mathieu[Perm](11).setwiseStabilizer(2, 9).order shouldBe 144
    }
    {
      import PGrp.default._ // TODO: use fixed seed
      Mathieu[Perm](11).setwiseStabilizer(2, 9).order shouldBe 144
    }
  }

}
