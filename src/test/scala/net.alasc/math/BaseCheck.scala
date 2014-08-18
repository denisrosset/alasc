package net.alasc.math

import org.scalacheck._
import scala.util.Random
import org.scalatest.FunSuite
import bsgs._
import algorithms._
import net.alasc.syntax.check._
import net.alasc.syntax.subgroup._

object BaseGenerators {
  import MathieuGroups._
  val groupAndBase = for {
    (generators, order) <- Gen.oneOf(M11, M12, M24, RubikCube.group)
    n <- Gen.choose(3, 6)
    baseAnsatz <- Gen.listOfN(n, Gen.choose(0, 49))
    base = baseAnsatz.distinct
    seed <- Gen.choose(0, 1000)
    useRandom <- Gen.oneOf(true, false)
    alg = if (useRandom) BasicAlgorithms.randomized[Perm](new scala.util.Random(seed)) else BasicAlgorithms.deterministic[Perm]
    mutableChain = alg.completeChainFromGeneratorsAndOrder(generators, order)
  } yield (mutableChain, base, alg, seed)
}
object BaseSpec extends Properties("BSGS") {
  import BaseGenerators._

  property("Base swap of the two first points") = Prop.forAll(groupAndBase) {
    case (mutableChain, base, alg, seed) => {
      implicit def nb = alg.nodeBuilder
      val order = mutableChain.start.next.order
      val node1 = mutableChain.mutable(mutableChain.start.next.nodesNext(0))
      val node2 = mutableChain.mutable(mutableChain.start.next.nodesNext(1))
      alg.baseSwap(mutableChain, node1, node2)
      mutableChain.start.next.order == order
/*      start.check
      modified.check
      val cbmut = alg.mutableChainCopy(modified.asInstanceOf[Node[Perm]])
      alg.changeBase(cbmut, cbmut.start, start.base)
      val comingBack = cbmut.toChain
      comingBack.check
      start.nodesNext.map(_.orbitSet).sameElements(comingBack.nodesNext.map(_.orbitSet))*/
    }
  }
}
