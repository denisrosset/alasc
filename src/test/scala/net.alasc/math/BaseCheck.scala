package net.alasc.math

import org.scalacheck._
import scala.util.Random
import org.scalatest.FunSuite
import bsgs._
import algorithms._
import net.alasc.syntax.check._
import net.alasc.syntax.subgroup._
import net.alasc.syntax.shiftablePermutation._

object BaseGenerators {
  import MathieuGroups._
  val mutableChainAlg = for {
    (generators, order) <- Gen.oneOf(M11, M12, M20, M21, M22, M23, RubikCube.group)
    seed <- Gen.choose(0, 1000)
    useRandom <- Gen.oneOf(true, false)
    alg = if (useRandom) BasicAlgorithms.randomized[Perm](new scala.util.Random(seed)) else BasicAlgorithms.deterministic[Perm]
  } yield  (alg.completeChainFromGeneratorsAndOrder(generators, order), alg)

  val mutableChainAlgExistingBasePoint = for {
    (mc, alg) <- mutableChainAlg
    k <- Gen.choose(0, mc.start.next.length - 1)
  } yield (mc, alg, mc.start.next.base(k))

  val mutableChainAlgSwapIndex = for {
    (mc, alg) <- mutableChainAlg
    index <- Gen.choose(0, mc.start.next.length - 2)
  } yield (mc, alg, index)

  val mutableChainAlgAndReorderedBase = for {
    (mc, alg) <- mutableChainAlg
    seq <- Gen.listOfN(mc.start.next.length, Gen.choose(0, 1000))
    newBase = (mc.start.next.base zip seq).sortBy(_._2).map(_._1)
  } yield (mc, alg, newBase)

  val mutableChainAlgAndNewBase = for {
    (mc, alg) <- mutableChainAlg
    newN <- Gen.choose(0, 8)
    newBaseAnsatz <- Gen.listOfN(newN, Gen.choose(0, 50))
  } yield (mc, alg, newBaseAnsatz)
}

object BaseSpec extends Properties("BSGS") {
  import BaseGenerators._

  property("Base swap of the two base points") = Prop.forAllNoShrink(mutableChainAlgSwapIndex) {
    case (mutableChain, alg, index) => {
      implicit def nb = alg.nodeBuilder
      val order = mutableChain.start.next.order
      val node1 = mutableChain.mutable(mutableChain.start.next.nodesNext(index))
      val node2 = mutableChain.mutable(mutableChain.start.next.nodesNext(index + 1))
      alg.baseSwap(mutableChain, node1, node2)
      mutableChain.start.next.order == order
    }
  }
  
  property("putExistingBasePointAfter") = Prop.forAllNoShrink(mutableChainAlgExistingBasePoint) {
    case (mutableChain, alg, beta) => {
      implicit def nb = alg.nodeBuilder
      val order = mutableChain.start.next.order
      alg.putExistingBasePointAfter(mutableChain, mutableChain.start, beta)
      mutableChain.start.next.order == order
    }
  }

  property("changeBase -- reordered base") = Prop.forAllNoShrink(mutableChainAlgAndReorderedBase) {
    case (mutableChain, alg, newBase) => {
      implicit def nb = alg.nodeBuilder
      val order = mutableChain.start.next.order
      alg.changeBase(mutableChain, mutableChain.start, newBase)
      mutableChain.start.next.order == order
    }
  }

  property("changeBase -- new base") = Prop.forAllNoShrink(mutableChainAlgAndReorderedBase) {
    case (mutableChain, alg, newBaseAnsatz) => {
      implicit def nb = alg.nodeBuilder
      val newBase = newBaseAnsatz.distinct
      val order = mutableChain.start.next.order
      alg.changeBase(mutableChain, mutableChain.start, newBase)
      mutableChain.start.next.order == order
    }
  }
}
