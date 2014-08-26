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

import net.alasc.algebra.{PermutationAction, Subgroup}
import net.alasc.syntax.check._
import net.alasc.util._

trait BaseChangeSwapConjugation[P] extends BaseAlgorithms[P] with BaseChangeGuided[P] {
  def changeBaseConjugation(mutableChain: MutableChain[P], guide: BaseGuide)(
    implicit action: PermutationAction[P]): InversePair[P] = {
    require(action eq mutableChain.start.action)
    @tailrec def rec(prev: StartOrNode[P], lastMutableStartOrNode: MutableStartOrNode[P], conj: InversePair[P]): InversePair[P] = {
      prev.next match {
        case IsMutableNode(mutableNode) =>
          val mutablePrev = mutableNode.prev
          val easyPoints = mutable.BitSet.empty
          mutableNode.foreachOrbit { k => easyPoints += (k <|+| conj.g) }
          val beta = guide.basePoint(easyPoints)
          val alpha = beta <|+| conj.gInv
          if (mutableNode.beta == alpha) { // TODO: check conjugation of k
            guide.moveToNext(beta, k => mutableNode.next.isFixed(k <|+| conj.g))
            rec(mutableNode, mutablePrev, conj) // replace mutablePrev by mutableNode ?
          } else if (mutableNode.inOrbit(alpha)) {
            val nextConj = mutableNode.uPair(alpha) |+| conj
            guide.moveToNext(beta, k => mutableNode.next.isFixed(k <|+| nextConj.g))
            rec(mutableNode, mutablePrev, nextConj)  // replace mutablePrev by mutableNode ?
          } else {
            val newNode = changeBasePointAfter(mutableChain, mutablePrev, alpha)
            guide.moveToNext(beta, k => newNode.next.isFixed(k <|+| conj.g))
            rec(newNode, mutablePrev, conj)
          }
        case node: Node[P] =>
          val easyPoints = mutable.BitSet.empty
          node.foreachOrbit { k => easyPoints += (k <|+| conj.g) }
          val beta = guide.basePoint(easyPoints)
          val alpha = beta <|+| conj.gInv
          if (node.beta == alpha) {
            guide.moveToNext(beta, k => node.next.isFixed(k <|+| conj.g))
            rec(node, lastMutableStartOrNode, conj)
          } else if (node.inOrbit(alpha)) {
            val nextConj = node.uPair(alpha) |+| conj
            guide.moveToNext(beta, k => node.next.isFixed(k <|+| nextConj.g))
            rec(node, lastMutableStartOrNode, nextConj)  // replace mutablePrev by mutableNode ?
          } else {
            val mutablePrev = mutableChain.mutableStartOrNode(prev, lastMutableStartOrNode)
            val newNode = changeBasePointAfter(mutableChain, mutablePrev, alpha)
            guide.moveToNext(beta, k => newNode.next.isFixed(k <|+| conj.g))
            rec(newNode, mutablePrev, conj)  // replace mutablePrev by mutableNode ?
          }
        case term: Term[P] => conj
      }
    }
    rec(mutableChain.start, mutableChain.start, algebra.id)
  }

  def changeBaseConjugation(mutableChain: MutableChain[P], newBase: Seq[Int])(
    implicit action: PermutationAction[P]): InversePair[P] = {
    @tailrec def rec(prev: StartOrNode[P], lastMutableStartOrNode: MutableStartOrNode[P], remaining: Iterator[Int], conj: InversePair[P]): InversePair[P] = {
      if (remaining.isEmpty) {
        cutRedundantAfter(mutableChain, prev)
        conj
      } else {
        val beta = remaining.next
        val alpha = beta <|+| conj.gInv
        prev.next match {
          case IsMutableNode(mutableNode) =>
            val mutablePrev = mutableNode.prev
            if (mutableNode.beta == alpha)
              rec(mutableNode, mutablePrev, remaining, conj)  // replace mutablePrev by mutableNode ?
            else if (mutableNode.inOrbit(alpha))
              rec(mutableNode, mutablePrev, remaining, mutableNode.uPair(alpha) |+| conj)  // replace mutablePrev by mutableNode ?
            else {
              val newNode = changeBasePointAfter(mutableChain, mutablePrev, alpha)
              rec(newNode, mutablePrev, remaining, conj)  // replace mutablePrev by newNode ?
            }
          case node: Node[P] =>
            if (node.beta == alpha)
              rec(node, lastMutableStartOrNode, remaining, conj)
            else if (node.inOrbit(alpha))
              rec(node, lastMutableStartOrNode, remaining, node.uPair(alpha) |+| conj)
            else {
              val mutablePrev = mutableChain.mutableStartOrNode(prev, lastMutableStartOrNode)
              val newNode = changeBasePointAfter(mutableChain, mutablePrev, alpha)
              rec(newNode, mutablePrev, remaining, conj)  // replace mutablePrev by mutableNode ?
            }
          case term: Term[P] =>
            val newNode = nodeBuilder.standalone(beta)
            val mutablePrev = mutableChain.mutableStartOrNode(prev, lastMutableStartOrNode)
            mutableChain.insertInChain(mutablePrev, term, newNode)
            rec(mutablePrev.next.asInstanceOf[Node[P]], mutablePrev, remaining, conj)
        }
      }
    }
    rec(mutableChain.start, mutableChain.start, newBase.iterator, algebra.id)
  }
  def changeBase(mutableChain: MutableChain[P], newBase: Seq[Int])(implicit action: PermutationAction[P]): Unit = {
    val conj = changeBaseConjugation(mutableChain, newBase)
    mutableChain.conjugate(conj)
  }

  def changeBase(mutableChain: MutableChain[P], guide: BaseGuide)(implicit action: PermutationAction[P]): Unit = {
    val conj = changeBaseConjugation(mutableChain, guide)
    mutableChain.conjugate(conj)
  }
}

