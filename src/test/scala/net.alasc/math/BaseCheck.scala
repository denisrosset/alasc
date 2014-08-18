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
    (generators, order) <- Gen.oneOf(M11, M12, M20, M21, M22, M23, RubikCube.group)
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

  property("Base swap of the two base points") = Prop.forAllNoShrink(groupAndBase) {
    case (mutableChain, base, alg, seed) => {
      implicit def nb = alg.nodeBuilder
      val order = mutableChain.start.next.order
      val index = scala.util.Random.nextInt(mutableChain.start.next.nodesNext.length - 1)
      val node1 = mutableChain.mutable(mutableChain.start.next.nodesNext(index))
      val node2 = mutableChain.mutable(mutableChain.start.next.nodesNext(index + 1))
      alg.baseSwap(mutableChain, node1, node2)
      mutableChain.start.next.order == order
    }
  }
  
  property("putExistingBasePointAfter") = Prop.forAllNoShrink(groupAndBase) {
    case (mutableChain, base, alg, seed) => {
      implicit def nb = alg.nodeBuilder
      val order = mutableChain.start.next.order
      alg.putExistingBasePointAfter(mutableChain, mutableChain.start, mutableChain.start.next.base(scala.util.Random.nextInt(mutableChain.start.next.base.length)))
      mutableChain.start.next.order == order
    }
  }
}
