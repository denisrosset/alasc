package net.alasc.math
package bsgs
package algorithms

import spire.algebra.{Group, Order}
import spire.syntax.group._
import spire.syntax.action._

import net.alasc.algebra.FaithfulPermutationAction

import metal.syntax._

trait BaseOrder[P] extends Order[Int] {
  def base: Seq[Int]
  implicit def action: FaithfulPermutationAction[P]
}

class BaseMapOrder[P](val base: Seq[Int], val reorderedMap: metal.FHashMap[Int, Int])(implicit val action: FaithfulPermutationAction[P]) extends BaseOrder[P] {
  def compare(a: Int, b: Int): Int =
    (reorderedMap.getOrElse(a, a).toLong - reorderedMap.getOrElse(b, b).toLong).signum.toInt
}

object BaseMapOrder {
  def apply[P](base: Seq[Int])(implicit action: FaithfulPermutationAction[P]) = {
    val reorderedMap = metal.MHashMap.empty[Int, Int]
    val iter = base.iterator
    var v = Int.MinValue + 1
    while (iter.hasNext) {
      val beta = iter.next
      reorderedMap.update(beta, v)
      v += 1
    }
    new BaseMapOrder(base, reorderedMap)
  }
}

object BaseOrder {
  def apply[P](base: Seq[Int])(implicit action: FaithfulPermutationAction[P]): BaseOrder[P] = BaseMapOrder[P](base)

  def orderedIterator[P](mutableChain: MutableChain[P])(implicit group: Group[P]): Iterator[P] = {
    implicit def action = mutableChain.start.action
    val bo = apply[P](mutableChain.start.next.base)
    def rec(chain: Chain[P], gPrev: P): Iterator[P] = chain match {
      case node: Node[P] =>
        val io = ImageOrder(bo, gPrev)
        for {
          b <- node.orbit.toSeq.sorted(Order.ordering(io)).toIterator
          gThis = node.u(b) |+| gPrev
          g <- rec(node.next, gThis)
        } yield g
      case _: Term[P] => Iterator(gPrev)
    }
    rec(mutableChain.start.next, group.id)
  }
}

final class ElementOrder[P](val baseOrder: BaseOrder[P]) extends Order[P] {
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

object ElementOrder {
  def apply[P](baseOrder: BaseOrder[P]): Order[P] = new ElementOrder(baseOrder)
}

final class ImageOrder[P](val baseOrder: BaseOrder[P], val g: P) extends Order[Int] {
  implicit def action =  baseOrder.action
  def compare(a: Int, b: Int): Int = baseOrder.compare(a <|+| g, b <|+| g)
}

object ImageOrder {
  def apply[P](baseOrder: BaseOrder[P], g: P): Order[Int] = new ImageOrder(baseOrder, g)
}

