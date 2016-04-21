package net.alasc.bsgs

import spire.algebra.{Group, Order}
import spire.syntax.action._
import spire.syntax.group._

import metal.syntax._

import net.alasc.algebra.FaithfulPermutationAction

trait BaseOrder[G, F <: FaithfulPermutationAction[G] with Singleton] extends Order[Int] {

  implicit def action: F

  def base: Seq[Int]


}

class BaseMapOrder[G, F <: FaithfulPermutationAction[G] with Singleton]
  (val base: Seq[Int], val reorderedMap: metal.generic.HashMap[Int, Int])(implicit val action: F) extends BaseOrder[G, F] {

  def compare(a: Int, b: Int): Int =
    (reorderedMap.getOrElse(a, a).toLong - reorderedMap.getOrElse(b, b).toLong).signum.toInt

}

object BaseMapOrder {

  def apply[G, F <: FaithfulPermutationAction[G] with Singleton](base: Seq[Int])(implicit action: F): BaseMapOrder[G, F] = {
    val reorderedMap = metal.mutable.HashMap.empty[Int, Int]
    val iter = base.iterator
    var v = Int.MinValue + 1
    while (iter.hasNext) {
      val beta = iter.next
      reorderedMap.update(beta, v)
      v += 1
    }
    new BaseMapOrder[G, F](base, reorderedMap)
  }

}

object BaseOrder {

  def apply[G, F <: FaithfulPermutationAction[G] with Singleton](base: Seq[Int])(implicit action: F): BaseOrder[G, F] =
    BaseMapOrder[G, F](base)

  def orderedIterator[G:Group, F <: FaithfulPermutationAction[G] with Singleton]
    (mutableChain: MutableChain[G, F]): Iterator[G] = {
    implicit def action: F = mutableChain.start.action
    val bo = apply[G, F](mutableChain.start.next.base)
    def rec(chain: Chain[G, F], gPrev: G): Iterator[G] = chain match {
      case node: Node[G, F] =>
        val io = ImageOrder(bo, gPrev)
        for {
          b <- node.orbit.toSeq.sorted(Order.ordering(io)).toIterator
          gThis = node.u(b) |+| gPrev
          g <- rec(node.next, gThis)
        } yield g
      case _: Term[G, F] => Iterator(gPrev)
    }
    rec(mutableChain.start.next, Group[G].id)
  }

}

final class ElementOrder[G, F <: FaithfulPermutationAction[G] with Singleton](val baseOrder: BaseOrder[G, F]) extends Order[G] {

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

  def apply[G, F <: FaithfulPermutationAction[G] with Singleton](baseOrder: BaseOrder[G, F]): Order[G] =
    new ElementOrder(baseOrder)

}

final class ImageOrder[G, F <: FaithfulPermutationAction[G] with Singleton]
  (val baseOrder: BaseOrder[G, F], val g: G) extends Order[Int] {

  implicit def action =  baseOrder.action

  def compare(a: Int, b: Int): Int = baseOrder.compare(a <|+| g, b <|+| g)

}

object ImageOrder {

  def apply[G, F <: FaithfulPermutationAction[G] with Singleton](baseOrder: BaseOrder[G, F], g: G): Order[Int] =
    new ImageOrder(baseOrder, g)
  
}

