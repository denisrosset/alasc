package net.alasc.math
package bsgs
package algorithms

import scala.annotation.tailrec

import spire.syntax.groupAction._

import net.alasc.algebra.{PermutationAction, Subgroup}
import net.alasc.syntax.check._
import net.alasc.util._

trait BaseChange[P] extends Algorithms[P] {
  /** Change the base after the element `after` in `mutableChain` by `newBase`.
    * 
    * @param mutableChain Mutable chain on which to perform the base change.
    * @param after        Element after which the base is replaced.
    * @param newBase      New base to use, will be extended if it is not a complete base.
    */
  def changeBase(mutableChain: MutableChain[P], newBase: Seq[Int])(implicit action: PermutationAction[P]): Unit
}

trait BaseAlgorithms[P] extends MutableAlgorithms[P] {
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
    findLastRedundant(mutableChain, afterThis.next).fold(0) { lastR =>
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
    }
}

trait BaseChangeSwap[P] extends BaseAlgorithms[P] with BaseSwap[P] {
  /** Finds an element such that `beta` is stabilized by the subgroup after the element. */  
  def findElemBeforeStabilizer(mutableChain: MutableChain[P], from: StartOrNode[P], beta: Int)(
    implicit action: PermutationAction[P]): StartOrNode[P] = {
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

  def insertNewBasePointAfter(mutableChain: MutableChain[P], afterThis: MutableStartOrNode[P], beta: Int)(
    implicit action: PermutationAction[P]): Unit = {
    require(beta >= 0)
    require(action == mutableChain.start.action)
    val insertAfter = mutableChain.mutableStartOrNode(findElemBeforeStabilizer(mutableChain, afterThis, beta))
    val newNode = nodeBuilder.standalone(beta)(mutableChain.start.action, algebra)
    mutableChain.insertInChain(insertAfter, insertAfter.next, newNode)
  }

  def changeBasePointAfter(mutableChain: MutableChain[P], afterThis: MutableStartOrNode[P], beta: Int)(
    implicit action: PermutationAction[P]): Unit = {
    require(beta >= 0)
    if (!putExistingBasePointAfter(mutableChain, afterThis, beta)) {
      insertNewBasePointAfter(mutableChain, afterThis, beta)
      assert(putExistingBasePointAfter(mutableChain, afterThis, beta))
    }
  }

  /** Shifts the existing `beta` at the node after `after`, if the chain already contains `basePoint`.
    * 
    * @return True if the chain contains `basePoint` and the shift was performed, `false` otherwise.
    */
  def putExistingBasePointAfter(mutableChain: MutableChain[P], after: MutableStartOrNode[P], beta: Int): Boolean = {
    findBasePoint(mutableChain, after.next, beta).fold(false) { toShift =>
      val mutableToShift = mutableChain.mutable(toShift, after)
      @tailrec def shift(pos: MutableNode[P]): Unit =
        if (pos.prev ne after) {
          pos.prev match {
            case prevNode: MutableNode[P] =>
              val (node1, node2) = baseSwap(mutableChain, prevNode, pos)
              shift(node1)
            case _: Start[P] => sys.error("mutableHere should be before mutableToShift")
          }
        }
      shift(mutableToShift)
      true
    }
  }

  def changeBase(mutableChain: MutableChain[P], newBase: Seq[Int])(implicit action: PermutationAction[P]): Unit = {
    require(action eq mutableChain.start.action)
    @tailrec def rec(prev: StartOrNode[P], lastMutableStartOrNode: MutableStartOrNode[P], remaining: Iterator[Int]): Unit =
      if (remaining.isEmpty) cutRedundantAfter(mutableChain, prev) else {
        val beta = remaining.next
        prev.next match {
          case IsMutableNode(mutableNode) =>
            val mutablePrev = mutableNode.prev
            if (mutableNode.beta == beta)
              rec(mutableNode, mutablePrev, remaining)
            else {
              changeBasePointAfter(mutableChain, mutablePrev, beta)
//              mutableChain.check
              rec(prev.next.asInstanceOf[Node[P]], mutablePrev, remaining)
            }
          case node: Node[P] =>
            if (node.beta == beta)
              rec(node, lastMutableStartOrNode, remaining)
            else {
              val mutablePrev = mutableChain.mutableStartOrNode(prev, lastMutableStartOrNode)
              changeBasePointAfter(mutableChain, mutablePrev, beta)
//              mutableChain.check
              rec(prev.next.asInstanceOf[Node[P]], mutablePrev, remaining)
            }
          case term: Term[P] =>
            val newNode = nodeBuilder.standalone(beta)
            val mutablePrev = mutableChain.mutableStartOrNode(prev, lastMutableStartOrNode)
            mutableChain.insertInChain(mutablePrev, term, newNode)
//            mutableChain.check
            rec(prev.next.asInstanceOf[Node[P]], mutablePrev, remaining)
        }
      }
    rec(mutableChain.start, mutableChain.start, newBase.iterator)
  }
}
/*
trait BaseChangeSwapConjugation[P] extends BaseAlgorithms[P] {
  }
}*/

trait BaseChangeFromScratch[P] extends BaseChange[P] with SchreierSims[P] {
  def changeBase(mutableChain: MutableChain[P], after: MutableStartOrNode[P], newBase: Seq[Int])(
    implicit action: PermutationAction[P]): Unit = {
    require(action == after.action)
    val tempChain = completeChainFromSubgroup(mutableChain.start.next, newBase)(after.action, implicitly[Subgroup[Chain[P], P]])
    mutableChain.replaceChain(mutableChain.start, tempChain.start)
  }
}
