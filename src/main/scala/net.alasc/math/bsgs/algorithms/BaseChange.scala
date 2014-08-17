package net.alasc.math
package bsgs
package algorithms

import scala.annotation.tailrec

import spire.syntax.groupAction._

import net.alasc.algebra.{PermutationAction, Subgroup}
import net.alasc.util._

trait BaseChange[P] extends Algorithms[P] {
  /** Change the base after the element `after` in `mutableChain` by `newBase`.
    * 
    * @param mutableChain Mutable chain on which to perform the base change.
    * @param after        Element after which the base is replaced.
    * @param newBase      New base to use, will be extended if it is not a complete base.
    */
  def changeBase(mutableChain: MutableChain[P], after: MutableStartOrNode[P], newBase: Seq[Int])(implicit action: PermutationAction[P]): Unit
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

  /** Removes redundant nodes in `mutableChain` after `node`.
    * 
    * @param mutableChain Mutable chain on which to perform the operation
    * @param afterThis    Element whose next elements are cleaned up
    * @return the number of removed elements
    */
  def cutRedundant(mutableChain: MutableChain[P], afterThis: StartOrNode[P]): Int =
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
  def findElemBeforeStabilizer(mutableChain: MutableChain[P], from: StartOrNode[P], beta: Int)(
    implicit action: PermutationAction[P]): StartOrNode[P] = {
    require(action == mutableChain.start.action)
    @tailrec def rec(chain: Chain[P], lastCandidate: StartOrNode[P]): StartOrNode[P] = chain match {
      case _: Term[P] => lastCandidate
      case node: Node[P] =>
        if (node.ownGenerators.exists(g => (beta <|+| g) != g))
          rec(node, node)
        else
          rec(node, lastCandidate)
    }
    rec(from.next, from)
  }

  def insertNewBasePoint(mutableChain: MutableChain[P], after: MutableStartOrNode[P], beta: Int)(
    implicit action: PermutationAction[P]): MutableNode[P] = {
    require(action == mutableChain.start.action)
    val insertAfter = mutableChain.mutableStartOrNode(findElemBeforeStabilizer(mutableChain, after, beta))
    val newNode = nodeBuilder.standalone(beta)(mutableChain.start.action, algebra)
    mutableChain.insertInChain(insertAfter, insertAfter.next, newNode)
    newNode
  }

  def changeBasePoint(mutableChain: MutableChain[P], here: MutableNode[P], beta: Int)(
    implicit action: PermutationAction[P]): MutableNode[P] =
    putExistingBasePointHere(mutableChain, here, beta).getOrElse {
      insertNewBasePoint(mutableChain, here, beta)
      putExistingBasePointHere(mutableChain, here, beta).getOrElse(sys.error("Insertion should always suceed"))
    }

  /** Shifts the existing `beta` at the node `here`, if the chain already contains `basePoint`.
    * 
    * @return The destination node `here`, or None if the chain does not contains `basePoint`.
    */
  def putExistingBasePointHere(mutableChain: MutableChain[P], here: MutableNode[P], beta: Int): Option[MutableNode[P]] = {
    findBasePoint(mutableChain, here, beta).fold[Option[MutableNode[P]]](None) { toShift =>
      val mutableHere = mutableChain.mutable(here)
      val mutableToShift = mutableChain.mutable(toShift)
      @tailrec def shift(pos: MutableNode[P]): Unit =
        if (pos ne mutableHere) {
          pos.prev match {
            case prevNode: MutableNode[P] =>
              baseSwap(mutableChain, prevNode)
              shift(prevNode)
            case _: Start[P] => sys.error("mutableHere should be before mutableToShift")
          }
        }
      shift(mutableToShift)
      Some(mutableHere)
    }
  }

  def changeBase(mutableChain: MutableChain[P], after: MutableStartOrNode[P], newBase: Seq[Int])(
    implicit action: PermutationAction[P]): Unit = {
    require(action eq mutableChain.start.action)
    @tailrec def rec(after: StartOrNode[P], lastMutableOrStart: MutableStartOrNode[P], remaining: Iterator[Int]): Unit =
      if (remaining.isEmpty) cutRedundant(mutableChain, after) else {
        val beta = remaining.next
        after.next match {
          case IsMutableNode(mutableNode) =>
            if (mutableNode.beta == beta)
              rec(mutableNode, mutableNode, remaining)
            else {
              val changedNode = changeBasePoint(mutableChain, mutableNode, beta)
              rec(changedNode, changedNode, remaining)
            }
          case node: Node[P] =>
            if (node.beta == beta)
              rec(node, lastMutableOrStart, remaining)
            else {
              val changedNode = changeBasePoint(mutableChain, mutableChain.mutable(node, lastMutableOrStart), beta)
              rec(changedNode, changedNode, remaining)
            }
          case term: Term[P] =>
            val newNode = nodeBuilder.standalone(beta)
            mutableChain.insertInChain(mutableChain.mutableStartOrNode(after, lastMutableOrStart), term, newNode)
            rec(newNode, newNode, remaining)
        }
      }
    rec(after, after, newBase.iterator)
  }
}

/*
trait BaseChangeSwapConjugation[P] extends BaseAlgorithms[P] {

}
 */
trait BaseChangeFromScratch[P] extends BaseChange[P] with SchreierSims[P] {
  def changeBase(mutableChain: MutableChain[P], after: MutableStartOrNode[P], newBase: Seq[Int])(
    implicit action: PermutationAction[P]): Unit = {
    require(action == after.action)
    val tempChain = completeChainFromSubgroup(after.next, newBase)(after.action, implicitly[Subgroup[Chain[P], P]])
    mutableChain.replaceChain(after, tempChain.start)
  }
}
