package net.alasc.math
package bsgs
package algorithms

import scala.annotation.tailrec

import spire.syntax.action._

import net.alasc.algebra.FaithfulPermutationAction
import net.alasc.util._

trait BaseAlgorithms[P] extends MutableAlgorithms[P] with BaseSwap[P] {
  /** Checks if there exist a base point with orbit size 1 in `mutableChain`, starting from `chain`. */
  @tailrec final def existsRedundantBasePoint(mutableChain: MutableChain[P], chain: Chain[P]): Boolean = chain match {
    case node: Node[P] =>
      if (node.orbitSize == 1)
        true
      else
        existsRedundantBasePoint(mutableChain, node.next)
    case _: Term[P] => false
  }

  /** Returns the node with base `basePoint` in `mutableChain` starting from `chain`, or `RefNone` if it cannot be found.  */
  @tailrec final def findBasePoint(mutableChain: MutableChain[P], chain: Chain[P], basePoint: Int): RefOption[Node[P]] = chain match {
    case node: Node[P] =>
      if (node.beta == basePoint)
        RefSome(node)
      else
        findBasePoint(mutableChain, node.next, basePoint)
    case _: Term[P] => RefNone
  }

  /** Finds the last redundant node in `mutableChain`, starting from `chain`. */
  def findLastRedundant(mutableChain: MutableChain[P], from: Chain[P]): RefOption[Node[P]] = {
    @tailrec def rec(chain: Chain[P], lastRedundantOption: RefOption[Node[P]]): RefOption[Node[P]] = chain match {
      case _: Term[P] => lastRedundantOption
      case node: Node[P] =>
        if (node.orbitSize == 1)
          rec(node.next, RefSome(node))
        else
          rec(node.next, lastRedundantOption)
    }
    rec(from, RefNone)
  }

  /** Removes redundant nodes in `mutableChain` after `afterThis`.
    * 
    * @param mutableChain Mutable chain on which to perform the operation
    * @param afterThis    Element whose next elements are cleaned up
    * @return the number of removed elements
    */
  def cutRedundantAfter(mutableChain: MutableChain[P], afterThis: StartOrNode[P]): Int =
    findLastRedundant(mutableChain, afterThis.next) match {
      case RefOption(lastR) =>
        @tailrec def eliminateRedundantTail(prev: StartOrNode[P], removed: Int): Int = prev.next match {
          case _: Term[P] => sys.error("lastR should have been encountered before")
          case node: Node[P] =>
            if (node.orbitSize == 1) {
              val isLastR = node eq lastR
              val mutablePrev = mutableChain.mutableStartOrNode(prev)
              mutableChain.remove(mutablePrev, node, node.next)
              if (!isLastR)
                eliminateRedundantTail(mutablePrev, removed + 1)
              else
                removed + 1
            } else
              eliminateRedundantTail(node, removed)
        }
        eliminateRedundantTail(afterThis, 0)
      case _ => 0
    }

  /** Finds an element such that `beta` is stabilized by the subgroup after the element. */
  def findElemBeforeStabilizer(mutableChain: MutableChain[P], from: StartOrNode[P], beta: Int)(
    implicit action: FaithfulPermutationAction[P]): StartOrNode[P] = {
    require(beta >= 0)
    require(action == mutableChain.start.action)
    @tailrec def rec(chain: Chain[P], lastCandidate: StartOrNode[P]): StartOrNode[P] = chain match {
      case _: Term[P] => lastCandidate
      case node: Node[P] =>
        if (node.ownGenerators.exists(g => (beta <|+| g) != beta))
          rec(node.next, node)
        else
          rec(node.next, lastCandidate)
    }
    rec(from.next, from)
  }

  /** Inserts a (non-existing) base point after the element `afterThis`. */
  def insertNewBasePointAfter(mutableChain: MutableChain[P], afterThis: MutableStartOrNode[P], beta: Int)(
    implicit action: FaithfulPermutationAction[P]): Unit = {
    require(beta >= 0)
    require(action == mutableChain.start.action)
    val insertAfter = mutableChain.mutableStartOrNode(findElemBeforeStabilizer(mutableChain, afterThis, beta))
    val newNode = nodeBuilder.standalone(beta)(mutableChain.start.action, algebra)
    mutableChain.insertInChain(insertAfter, insertAfter.next, newNode)
  }

  /** Change the base point after the element `afterThis` to `beta` and returns this node with base point `beta`. */
  def changeBasePointAfter(mutableChain: MutableChain[P], afterThis: MutableStartOrNode[P], beta: Int)(
    implicit action: FaithfulPermutationAction[P]): Node[P] =
    putExistingBasePointAfter(mutableChain, afterThis, beta).getOrElse {
      insertNewBasePointAfter(mutableChain, afterThis, beta)
      putExistingBasePointAfter(mutableChain, afterThis, beta).get
    }

  /** Shifts the existing `beta` at the node after `after`, if the chain already contains `basePoint`.
    * 
    * @return The shifted node with base point `beta` if the chain contains `basePoint` 
    and the shift was performed, `RefNone` otherwise.
    */
  def putExistingBasePointAfter(mutableChain: MutableChain[P], after: MutableStartOrNode[P], beta: Int): RefOption[Node[P]] = {
    findBasePoint(mutableChain, after.next, beta) match {
      case RefOption(toShift) =>
        val mutableToShift = mutableChain.mutable(toShift, after)
        @tailrec def shift(pos: MutableNode[P]): RefOption[Node[P]] =
          if (pos.prev ne after) {
            pos.prev match {
              case prevNode: MutableNode[P] =>
                val MutableNodeAndNext(node1, node2) = baseSwap(mutableChain, prevNode, pos)
                shift(node1)
              case _: Start[P] => sys.error("mutableHere should be before mutableToShift")
            }
          } else
            RefSome(pos)
        shift(mutableToShift)
      case _ => RefNone
    }
  }
}
