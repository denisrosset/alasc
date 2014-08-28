package net.alasc.math
package bsgs
package algorithms

import scala.annotation.tailrec

import spire.algebra.Order
import spire.syntax.groupAction._
import spire.syntax.group._

import net.alasc.algebra.{FaithfulPermutationAction, Subgroup}
import net.alasc.syntax.check._
import net.alasc.util._

trait Orders[P] extends Algorithms[P] {
  trait BaseOrder extends Order[Int] {
    def base: Seq[Int]
    implicit def action: FaithfulPermutationAction[P]
  }

  def baseOrder(base: Seq[Int])(implicit action: FaithfulPermutationAction[P]): BaseOrder

  class ElementOrder(val baseOrder: BaseOrder) extends Order[P] {
    def compare(a: P, b: P): Int = {
      import baseOrder.action
      val iter = baseOrder.base.iterator
      while (iter.hasNext) {
        val beta = iter.next
        val ord = baseOrder.compare(beta <|+| a, beta <|+| b)
        if (ord != 0)
          return ord
      }
      0
    }
  }

  def elementOrder(baseOrder: BaseOrder): Order[P]

  def imageOrder(baseOrder: BaseOrder, g: P): Order[Int]

  def orderedIterator(mutableChain: MutableChain[P]): Iterator[P] = {
    implicit def action = mutableChain.start.action
    val bo = baseOrder(mutableChain.start.next.base)
    def rec(chain: Chain[P], gPrev: P): Iterator[P] = chain match {
      case node: Node[P] =>
        val io = imageOrder(bo, gPrev)
        for {
          b <- node.orbit.toSeq.sorted(Order.ordering(io)).toIterator
          gThis = node.u(b) |+| gPrev
          g <- rec(node.next, gThis)
        } yield g
      case _: Term[P] => Iterator(gPrev)
    }
    rec(mutableChain.start.next, algebra.id)
  }
}

trait OrdersImpl[P] extends Orders[P] {
  class BaseMapOrder(val base: Seq[Int], val reorderedMap: debox.Map[Int, Int])(implicit val action: FaithfulPermutationAction[P]) extends BaseOrder {
    def compare(a: Int, b: Int): Int =
      (reorderedMap.getOrElse(a, a).toLong - reorderedMap.getOrElse(b, b).toLong).signum.toInt
  }

  def baseOrder(base: Seq[Int])(implicit action: FaithfulPermutationAction[P]) = {
    val reorderedMap = debox.Map.empty[Int, Int]
    val iter = base.iterator
    var v = Int.MinValue + 1
    while (iter.hasNext) {
      val beta = iter.next
      reorderedMap.update(beta, v)
      v += 1
    }
    new BaseMapOrder(base, reorderedMap)
  }

  final class ElementOrder(val baseOrder: BaseOrder) extends Order[P] {
    def compare(a: P, b: P): Int = {
      implicit def action =  baseOrder.action
      val iter = baseOrder.base.iterator
      while (iter.hasNext) {
        val beta = iter.next
        val ord = baseOrder.compare(beta <|+| a, beta <|+| b)
        if (ord != 0)
          return ord
      }
      0
    }
  }

  def elementOrder(baseOrder: BaseOrder): Order[P] =
    new ElementOrder(baseOrder)

  final class ImageOrder(val baseOrder: BaseOrder, val g: P) extends Order[Int] {
    implicit def action =  baseOrder.action
    def compare(a: Int, b: Int): Int = baseOrder.compare(a <|+| g, b <|+| g)
  }

  def imageOrder(baseOrder: BaseOrder, g: P) = new ImageOrder(baseOrder, g)
}
