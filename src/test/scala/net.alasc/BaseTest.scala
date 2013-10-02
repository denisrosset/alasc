package net.alasc

import org.scalacheck._
import scala.util.Random
import org.scalatest.FunSuite

object BaseGenerators {
  val groupAndBase = for {
    g <- Gen.oneOf(M11.g, M12.g, M24.g)
    n <- Gen.choose(3, 6)
    method <- Gen.oneOf(BaseSwapOnly, BaseFromScratch) //BaseSwapAndConjugation, BaseSwapAndConjugation) //BaseSwapOnly, , BaseFromScratch)
    base <- Gen.listOfN(n, Gen.choose(1, g.identity.size)) if base.distinct.size == base.size
    seed <- Gen.choose(0, 1000)
    useRandom <- Gen.oneOf(true, false)
    options = GroupOptions.default.copy(useRandomizedAlgorithms = useRandom, baseChangeStrategy = method, randomGenerator = new scala.util.Random(seed))
  } yield (g.withOptions(options), base.map(Dom._1(_)), g.identity.size, method, seed, useRandom)
}

object BaseSpec extends Properties("GroupBSGSBase") {
  import BaseGenerators._

  property("After changing base and changing back, transversal orbits should be the same") = Prop.forAll(groupAndBase) {
    case (group, newBase, dim, method, seed, useRandom) => {
      val start = group.bsgs
      start.check
      val modified = start.withBase(newBase)
      modified.check
      val comingBack = modified.withBase(start.base)
      comingBack.check
      start.transversals.map(_.orbit).sameElements(comingBack.transversals.map(_.orbit))
    }
  }
}

class BaseSuite extends FunSuite {
  test("Other bug") {
    import net.alasc._
    import Dom.OneBased._
    val newOptions = GroupOptions.default.copy(baseChangeStrategy = BaseSwapOnly, randomGenerator = new scala.util.Random(1), useRandomizedAlgorithms = true)
    val group = M24.g.withOptions(newOptions)
    val newBase = List(6, 18, 22).map(Dom._1(_))
    val start = group.bsgs
    val modified = start.withBase(newBase)
    modified.check
    val comingBack = modified.withBase(start.base)
    comingBack.check
    assert(start.transversals.map(_.orbit).sameElements(comingBack.transversals.map(_.orbit)))
  }
  test("Bug in base swap 1") {
    import Dom.OneBased._
    val newOptions = GroupOptions.default.copy(baseChangeStrategy = BaseSwapOnly, randomGenerator = new scala.util.Random(10), useRandomizedAlgorithms = true)
    val g = M11.g.withOptions(newOptions)

    val g1 = List(Perm(11)(2,7,6)(4,11,8)(5,9,10), Perm(11)(4,6,10,7)(5,11,8,9), Perm(11)(4,8,10,5)(6,11,7,9))
    val g2 = List(Perm(11)(4,6,10,7)(5,11,8,9), Perm(11)(4,8,10,5)(6,11,7,9))
    val g3 = Nil
    val trv1 = g.makeTransversal(2, g1)
    val trv2 = g.makeTransversal(6, g2)
    val trv3 = g.makeTransversal(4, g3)
    val bsgs = new g.BSGSNode(trv1, g1, new g.BSGSNode(trv2, g2, new g.BSGSNode(trv3, g3, new g.BSGSTerminal)))
    val swapped = bsgs.baseSwap
    swapped.check
  }


  test("Bug in base swap") {
    val newOptions = GroupOptions.default.copy(baseChangeStrategy = BaseSwapOnly, randomGenerator = new scala.util.Random(10), useRandomizedAlgorithms = false)
    val group = M11.g.withOptions(newOptions)
    val newBase = List(6, 4, 9, 2).map(Dom._1(_))
    val start = group.bsgs
    start.check
    val modified = start.withBase(newBase)
    val comingBack = modified.withBase(start.base)
    assert(start.transversals.map(_.orbit).sameElements(comingBack.transversals.map(_.orbit)))
  }
}
