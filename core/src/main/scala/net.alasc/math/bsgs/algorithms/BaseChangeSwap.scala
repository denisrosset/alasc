package net.alasc.math
package bsgs
package algorithms

import scala.annotation.tailrec

import net.alasc.algebra.FaithfulPermutationAction
import net.alasc.math.guide.BaseGuide

trait BaseChangeSwap[P] extends BaseAlgorithms[P] with BaseChange[P] {
  def changeBaseSameAction(mutableChain: MutableChain[P], guide: BaseGuide)(implicit action: FaithfulPermutationAction[P]): Unit = {
    val iter = guide.iterator
    require(action eq mutableChain.start.action)
    @tailrec def rec(prev: StartOrNode[P], lastMutableStartOrNode: MutableStartOrNode[P]): Unit = {
      if (prev.next.nodesNext.forall(_.orbitSize == 1) || !iter.hasNext)
        cutRedundantAfter(mutableChain, prev)
      else prev.next match {
          case IsMutableNode(mutableNode) =>
            val mutablePrev = mutableNode.prev
            val beta = iter.next(mutableNode.beta, Set(mutableNode.beta), mutableNode.isFixed(_))
            if (mutableNode.beta == beta)
              rec(mutableNode, mutablePrev)
            else {
              val newNode = changeBasePointAfter(mutableChain, mutablePrev, beta)
              rec(newNode, mutablePrev)
            }
          case node: Node[P] =>
            val beta = iter.next(node.beta, Set(node.beta), node.isFixed(_))
            if (node.beta == beta)
              rec(node, lastMutableStartOrNode)
            else {
              val mutablePrev = mutableChain.mutableStartOrNode(prev, lastMutableStartOrNode)
              val newNode = changeBasePointAfter(mutableChain, mutablePrev, beta)
              rec(newNode, mutablePrev)
            }
          case term: Term[P] => // finished
        }
    }
    rec(mutableChain.start, mutableChain.start)
  }
}
