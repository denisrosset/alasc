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

trait BaseChangeSwap[P] extends BaseAlgorithms[P] with BaseChangeGuided[P] {
  def changeBase(mutableChain: MutableChain[P], guide: BaseGuide)(implicit action: PermutationAction[P]): Unit = {
    require(action eq mutableChain.start.action)
    @tailrec def rec(prev: StartOrNode[P], lastMutableStartOrNode: MutableStartOrNode[P]): Unit =
        prev.next match {
          case IsMutableNode(mutableNode) =>
            val mutablePrev = mutableNode.prev
            val beta = guide.basePoint(Set(mutableNode.beta))
            if (mutableNode.beta == beta) {
              guide.moveToNext(beta, mutableNode.next.isFixed(_))
              rec(mutableNode, mutablePrev)
            }
            else {
              val newNode = changeBasePointAfter(mutableChain, mutablePrev, beta)
              guide.moveToNext(beta, newNode.next.isFixed(_))
              rec(newNode, mutablePrev)
            }
          case node: Node[P] =>
            val beta = guide.basePoint(Set(node.beta))
            if (node.beta == beta) {
              guide.moveToNext(beta, node.next.isFixed(_))
              rec(node, lastMutableStartOrNode)
            } else {
              val mutablePrev = mutableChain.mutableStartOrNode(prev, lastMutableStartOrNode)
              val newNode = changeBasePointAfter(mutableChain, mutablePrev, beta)
              guide.moveToNext(beta, newNode.next.isFixed(_))
              rec(newNode, mutablePrev)
            }
          case term: Term[P] => // finished
        }
    rec(mutableChain.start, mutableChain.start)
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
              val newNode = changeBasePointAfter(mutableChain, mutablePrev, beta)
              rec(newNode, mutablePrev, remaining)
            }
          case node: Node[P] =>
            if (node.beta == beta)
              rec(node, lastMutableStartOrNode, remaining)
            else {
              val mutablePrev = mutableChain.mutableStartOrNode(prev, lastMutableStartOrNode)
              val newNode = changeBasePointAfter(mutableChain, mutablePrev, beta)
              rec(newNode, mutablePrev, remaining)
            }
          case term: Term[P] =>
            val newNode = nodeBuilder.standalone(beta)
            val mutablePrev = mutableChain.mutableStartOrNode(prev, lastMutableStartOrNode)
            mutableChain.insertInChain(mutablePrev, term, newNode)
            rec(newNode, mutablePrev, remaining)
        }
      }
    rec(mutableChain.start, mutableChain.start, newBase.iterator)
  }
}
