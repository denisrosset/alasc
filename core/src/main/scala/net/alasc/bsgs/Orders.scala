package net.alasc.bsgs

import spire.algebra.{Group, Order}
import spire.syntax.action._
import spire.syntax.group._

import metal.syntax._

import net.alasc.algebra.PermutationAction
import net.alasc.syntax.group._

trait BaseOrder[G, A <: PermutationAction[G] with Singleton] extends Order[Int] {

  implicit def action: A

  def base: Seq[Int]


}

class BaseMapOrder[G, A <: PermutationAction[G] with Singleton]
  (val base: Seq[Int], val reorderedMap: metal.generic.HashMap[Int, Int])(implicit val action: A) extends BaseOrder[G, A] {

  def compare(a: Int, b: Int): Int =
    (reorderedMap.getOrElse(a, a).toLong - reorderedMap.getOrElse(b, b).toLong).signum.toInt

}

object BaseMapOrder {

  def apply[G, A <: PermutationAction[G] with Singleton](base: Seq[Int])(implicit action: A): BaseMapOrder[G, A] = {
    val reorderedMap = metal.mutable.HashMap.empty[Int, Int]
    val iter = base.iterator
    var v = Int.MinValue + 1
    while (iter.hasNext) {
      val beta = iter.next
      reorderedMap.update(beta, v)
      v += 1
    }
    new BaseMapOrder[G, A](base, reorderedMap)
  }

}

object BaseOrder {

  def apply[G, A <: PermutationAction[G] with Singleton](base: Seq[Int])(implicit action: A): BaseOrder[G, A] =
    BaseMapOrder[G, A](base)

  def orderedIterator[G:Group, A <: PermutationAction[G] with Singleton]
    (mutableChain: MutableChain[G, A]): Iterator[G] = {
    implicit def action: A = mutableChain.start.action
    val bo = apply[G, A](mutableChain.start.next.base)
    def rec(chain: Chain[G, A], gPrev: G): Iterator[G] = chain match {
      case node: Node[G, A] =>
        val io = ImageOrder(bo, gPrev)
        for {
          b <- node.orbit.toSeq.sorted(io.toOrdering).toIterator
          gThis = node.u(b) |+| gPrev
          g <- rec(node.next, gThis)
        } yield g
      case _: Term[G, A] => Iterator(gPrev)
    }
    rec(mutableChain.start.next, Group[G].id)
  }

}

final class ElementOrder[G, A <: PermutationAction[G] with Singleton](val baseOrder: BaseOrder[G, A]) extends Order[G] {

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

  def apply[G, A <: PermutationAction[G] with Singleton](baseOrder: BaseOrder[G, A]): Order[G] =
    new ElementOrder(baseOrder)

}

final class ImageOrder[G, A <: PermutationAction[G] with Singleton]
  (val baseOrder: BaseOrder[G, A], val g: G) extends Order[Int] {

  implicit def action =  baseOrder.action

  def compare(a: Int, b: Int): Int = baseOrder.compare(a <|+| g, b <|+| g)

}

object ImageOrder {

  def apply[G, A <: PermutationAction[G] with Singleton](baseOrder: BaseOrder[G, A], g: G): Order[Int] =
    new ImageOrder(baseOrder, g)
  
}

