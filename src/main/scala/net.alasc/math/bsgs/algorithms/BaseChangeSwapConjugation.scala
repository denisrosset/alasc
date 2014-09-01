package net.alasc.math
package bsgs
package algorithms

import scala.annotation.tailrec

import scala.collection.BitSet
import scala.collection.immutable
import scala.collection.mutable

import spire.algebra.Order
import spire.syntax.groupAction._
import spire.syntax.group._

import net.alasc.algebra.{FaithfulPermutationAction, Subgroup}
import net.alasc.syntax.check._
import net.alasc.util._

trait BaseChangeSwapConjugation[P] extends BaseAlgorithms[P] with BaseChange[P] {
  def changeBaseConjugation(mutableChain: MutableChain[P], guide: BaseGuide)(
    implicit action: FaithfulPermutationAction[P]): InversePair[P] = {
    val iter = guide.iterator
    require(action eq mutableChain.start.action)
    @tailrec def rec(prev: StartOrNode[P], lastMutableStartOrNode: MutableStartOrNode[P], conj: InversePair[P]): InversePair[P] = {
      if (prev.next.nodesNext.forall(_.orbitSize == 1) || !iter.hasNext) {
        cutRedundantAfter(mutableChain, prev)
        conj
      } else prev.next match {
        case IsMutableNode(mutableNode) =>
          val mutablePrev = mutableNode.prev
          val easyPoints = mutable.BitSet.empty
          mutableNode.foreachOrbit { k => easyPoints += (k <|+| conj.g) }
          val beta = iter.next(mutableNode.beta <|+| conj.g, easyPoints, k => mutableNode.isFixed(k <|+| conj.gInv))
          val alpha = beta <|+| conj.gInv
          if (mutableNode.beta == alpha) // TODO: check conjugation of k
            rec(mutableNode, mutablePrev, conj) // replace mutablePrev by mutableNode ?
          else if (mutableNode.inOrbit(alpha)) {
            val nextConj = mutableNode.uPair(alpha) |+| conj
            rec(mutableNode, mutablePrev, nextConj)  // replace mutablePrev by mutableNode ?
          } else {
            val newNode = changeBasePointAfter(mutableChain, mutablePrev, alpha)
            rec(newNode, mutablePrev, conj)
          }
        case node: Node[P] =>
          val easyPoints = mutable.BitSet.empty
          node.foreachOrbit { k => easyPoints += (k <|+| conj.g) }
          val beta = iter.next(node.beta <|+| conj.g, easyPoints, k => node.isFixed(k <|+| conj.gInv))
          val alpha = beta <|+| conj.gInv
          if (node.beta == alpha)
            rec(node, lastMutableStartOrNode, conj)
          else if (node.inOrbit(alpha)) {
            val nextConj = node.uPair(alpha) |+| conj
            rec(node, lastMutableStartOrNode, nextConj)  // replace mutablePrev by mutableNode ?
          } else {
            val mutablePrev = mutableChain.mutableStartOrNode(prev, lastMutableStartOrNode)
            val newNode = changeBasePointAfter(mutableChain, mutablePrev, alpha)
            rec(newNode, mutablePrev, conj)  // replace mutablePrev by mutableNode ?
          }
        case term: Term[P] => conj
      }
    }
    rec(mutableChain.start, mutableChain.start, algebra.id)
  }

  def changeBaseSameAction(mutableChain: MutableChain[P], guide: BaseGuide)(implicit action: FaithfulPermutationAction[P]): Unit = {
    val conj = changeBaseConjugation(mutableChain, guide)
    mutableChain.conjugate(conj)
  }
}
