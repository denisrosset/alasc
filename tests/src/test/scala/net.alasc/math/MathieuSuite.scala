package net.alasc.math

import org.scalatest.{FunSuite, NonImplicitAssertions, Matchers, EqMatchers}
import bsgs._
import net.alasc.syntax.subgroup._

object MathieuGroups {
  val M10 = Seq(Perm(1,2)(3,4), Perm(1,2,3,5)(4,6)) -> BigInt(360)
  val M11 = Seq(Perm(1,2,3,4,5,6,7,8,9,10,11), Perm(3,7,11,8)(4,10,5,6)) -> BigInt(7920)
  val M12 = Seq(Perm(1,2,3,4,5,6,7,8,9,10,11), Perm(1,12)(2,11)(3,6)(4,8)(5,9)(7,10), Perm(3,7,11,8)(4,10,5,6)) -> BigInt(95040)
  val M20 = Seq(Perm(1,2,4,3)(5,11,7,12)(6,13)(8,14)(9,15,10,16)(17,19,20,18),
    Perm(2,5,6)(3,7,8)(4,9,10)(11,17,12)(13,16,18)(14,15,19)) -> BigInt(960)
  val M21 = Seq(Perm(1,2)(4,6)(5,7)(8,12)(9,14)(10,15)(11,17)(13,19),
    Perm(2,3,5,4)(6,8,13,9)(7,10,16,11)(12,18)(14,20,21,15)(17,19)) -> BigInt(20160)
  val M22 = Seq(Perm(1,13)(2,8)(3,16)(4,12)(6,22)(7,17)(9,10)(11,14),
    Perm(1,22,3,21)(2,18,4,13)(5,12)(6,11,7,15)(8,14,20,10)(17,19)) -> BigInt(443520)
  val M23 = Seq(Perm(1,2)(3,4)(7,8)(9,10)(13,14)(15,16)(19,20)(21,22),
    Perm(1,16,11,3)(2,9,21,12)(4,5,8,23)(6,22,14,18)(13,20)(15,17)) -> BigInt(10200960)
  val M24 = Seq(Perm("0123456789ABCDEFGHIJKLM")("N"),
    Perm("0N")("1M")("2B")("3F")("4H")("59")("6J")("7D")("8K")("AG")("CL")("EI"),
    Perm("2G968")("3CDI4")("7HABM")("EJLKF")) -> BigInt(244823040)
  val all: Seq[(Seq[Perm], BigInt)] = Seq(M10, M11, M12, M20, M21, M22, M23, M24)
}

class MathieuSuite extends FunSuite with NonImplicitAssertions with Matchers {
  test("Mathieu group constructions have correct order, using deterministic Schreier-Sims") {
    val alg = algorithms.BasicAlgorithms.deterministic[Perm]
    MathieuGroups.all.foreach {
      case (gens, order) => alg.completeChainFromGenerators(gens).toChain.order shouldBe order
    }
  }

  test("Mathieu group construction works using randomized Schreier-Sims") {
    val alg = algorithms.BasicAlgorithms.randomized[Perm]()
    MathieuGroups.all.foreach {
      case (gens, order) => alg.completeChainFromGeneratorsAndOrder(gens, order).toChain.order shouldBe order
    }
  }

  test("Mathieu group 22 one-point stabilizers have order 20160") {
    (1 to 22).foreach { i =>
      Grp(MathieuGroups.M22._1: _*).stabilizer(i)._1.order shouldBe 20160
    }
  }

  test("Mathieu group 11 one-point stabilizers have order 720") {
    (1 to 11).foreach { i =>
      Grp(MathieuGroups.M11._1: _*).stabilizer(i)._1.order shouldBe 720
    }
  }

  test("Mathieu group 11 stabilizer of {2,9} has order 144") {
    Grp(MathieuGroups.M11._1: _*).setwiseStabilizer(2, 9).order shouldBe 144
  }
}
