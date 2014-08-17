package net.alasc.math

import org.scalatest.{FunSuite, NonImplicitAssertions, Matchers, EqMatchers}
import bsgs._
import net.alasc.syntax.subgroup._

object MathieuGroups {
  val M11 = Seq(Perm(1,2,3,4,5,6,7,8,9,10,11), Perm(3,7,11,8)(4,10,5,6)) -> BigInt(7920)
  val M12 = Seq(Perm(1,2,3,4,5,6,7,8,9,10,11), Perm(1,12)(2,11)(3,6)(4,8)(5,9)(7,10), Perm(3,7,11,8)(4,10,5,6)) -> BigInt(95040)
  val M24 = Seq(Perm("0123456789ABCDEFGHIJKLM")("N"),
    Perm("0N")("1M")("2B")("3F")("4H")("59")("6J")("7D")("8K")("AG")("CL")("EI"),
    Perm("2G968")("3CDI4")("7HABM")("EJLKF")) -> BigInt(244823040)
  val all: Seq[(Seq[Perm], BigInt)] = Seq(M11, M12, M24)
}

class MathieuSuite extends FunSuite with NonImplicitAssertions with Matchers {
  test("Mathieu group construction is correct for M11, M12 and M24, using deterministic Schreier-Sims") {
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
}
