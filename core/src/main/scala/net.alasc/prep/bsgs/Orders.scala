package net.alasc.prep.bsgs

import spire.algebra.{Group, Order}
import spire.syntax.group._
import spire.syntax.action._

import net.alasc.algebra.FaithfulPermutationAction

import metal.{FHashMap, MHashMap}
import metal.syntax._

trait BaseOrder[G] extends Order[Int] {

  def base: Seq[Int]

  implicit def action: FaithfulPermutationAction[G]

}

class BaseMapOrder[G](val action: FaithfulPermutationAction[G], val base: Seq[Int], val reorderedMap: FHashMap[Int, Int]) extends BaseOrder[G] {

  def compare(a: Int, b: Int): Int =
    (reorderedMap.getOrElse(a, a).toLong - reorderedMap.getOrElse(b, b).toLong).signum.toInt

}

object BaseMapOrder {

  def apply[G](action: FaithfulPermutationAction[G], base: Seq[Int]) = {
    val reorderedMap = MHashMap.empty[Int, Int]
    val iter = base.iterator
    var v = Int.MinValue + 1
    while (iter.hasNext) {
      val beta = iter.next
      reorderedMap.update(beta, v)
      v += 1
    }
    new BaseMapOrder(action, base, reorderedMap)
  }

}

object BaseOrder {

  def apply[G](action: FaithfulPermutationAction[G], base: Seq[Int]): BaseOrder[G] = BaseMapOrder[G](action, base)

  def orderedIterator[G:Group](mutableChain: MutableChain[G]): Iterator[G] = {
    implicit def action = mutableChain.start.action
    val bo = apply[G](action, mutableChain.start.next.base)
    def rec(chain: Chain[G], gPrev: G): Iterator[G] = chain match {
      case node: Node[G] =>
        val io = ImageOrder(bo, gPrev)
        for {
          b <- node.orbit.toSeq.sorted(Order.ordering(io)).toIterator
          gThis = node.u(b) |+| gPrev
          g <- rec(node.next, gThis)
        } yield g
      case _: Term[G] => Iterator(gPrev)
    }
    rec(mutableChain.start.next, Group[G].id)
  }

}

final class ElementOrder[G](val baseOrder: BaseOrder[G]) extends Order[G] {

  def compare(a: G, b: G): Int = {
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

  def apply[G](baseOrder: BaseOrder[G]): Order[G] = new ElementOrder(baseOrder)

}

final class ImageOrder[G](val baseOrder: BaseOrder[G], val g: G) extends Order[Int] {

  implicit def action =  baseOrder.action

  def compare(a: Int, b: Int): Int = baseOrder.compare(a <|+| g, b <|+| g)

}

object ImageOrder {

  def apply[G](baseOrder: BaseOrder[G], g: G): Order[Int] = new ImageOrder(baseOrder, g)
  
}

