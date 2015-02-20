package net.alasc.math
package bsgs
package algorithms

import scala.annotation.tailrec
import scala.collection.mutable

import spire.syntax.group._
import spire.syntax.action._

import net.alasc.algebra.FaithfulPermutationAction
import net.alasc.math.guide.BaseGuide

trait BaseChangeSwapConjugation[P] extends BaseAlgorithms[P] with BaseChange[P] {
  def changeBaseConjugation(mutableChain: MutableChain[P], guide: BaseGuide)(
    implicit action: FaithfulPermutationAction[P]): (P, P) = {
    val iter = guide.iterator
    require(action eq mutableChain.start.action)
    @tailrec def rec(prev: StartOrNode[P], lastMutableStartOrNode: MutableStartOrNode[P], conj: P, conjInv: P): (P, P) = {
      if (prev.next.nodesNext.forall(_.orbitSize == 1) || !iter.hasNext) {
        cutRedundantAfter(mutableChain, prev)
        (conj, conjInv)
      } else prev.next match {
        case IsMutableNode(mutableNode) =>
          val mutablePrev = mutableNode.prev
          val easyPoints = mutable.BitSet.empty
          mutableNode.foreachOrbit { k => easyPoints += (k <|+| conj) }
          val beta = iter.next(mutableNode.beta <|+| conj, easyPoints, k => mutableNode.isFixed(k <|+| conjInv))
          val alpha = beta <|+| conjInv
          if (mutableNode.beta == alpha)
            rec(mutableNode, mutablePrev, conj, conjInv)
          else if (mutableNode.inOrbit(alpha)) {
            val nextConj = mutableNode.u(alpha) |+| conj
            val nextConjInv = conjInv |+| mutableNode.uInv(alpha)
            rec(mutableNode, mutablePrev, nextConj, nextConjInv)
          } else {
            val newNode = changeBasePointAfter(mutableChain, mutablePrev, alpha)
            rec(newNode, mutablePrev, conj, conjInv)
          }
        case node: Node[P] =>
          val easyPoints = mutable.BitSet.empty
          node.foreachOrbit { k => easyPoints += (k <|+| conj) }
          val beta = iter.next(node.beta <|+| conj, easyPoints, k => node.isFixed(k <|+| conjInv))
          val alpha = beta <|+| conjInv
          if (node.beta == alpha)
            rec(node, lastMutableStartOrNode, conj, conjInv)
          else if (node.inOrbit(alpha)) {
            val nextConj = node.u(alpha) |+| conj
            val nextConjInv = conjInv |+| node.uInv(alpha)
            rec(node, lastMutableStartOrNode, nextConj, nextConjInv)
          } else {
            val mutablePrev = mutableChain.mutableStartOrNode(prev, lastMutableStartOrNode)
            val newNode = changeBasePointAfter(mutableChain, mutablePrev, alpha)
            rec(newNode, mutablePrev, conj, conjInv)
          }
        case term: Term[P] => (conj, conjInv)
      }
    }
    rec(mutableChain.start, mutableChain.start, algebra.id, algebra.id)
  }

  def changeBaseSameAction(mutableChain: MutableChain[P], guide: BaseGuide)(implicit action: FaithfulPermutationAction[P]): Unit = {
    val (g, gInv) = changeBaseConjugation(mutableChain, guide)
    mutableChain.conjugate(g, gInv)
  }
}
