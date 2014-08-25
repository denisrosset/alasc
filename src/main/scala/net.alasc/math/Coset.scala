package net.alasc.math

import spire.algebra.Order
import spire.syntax.group._
import spire.syntax.groupAction._

import net.alasc.algebra._
import net.alasc.syntax.subgroup._

import bsgs._

class RightCoset[G](grpH: Grp[G], g: G) {
  implicit def algebra = grpH.algebra
  def contains(el: G) = grpH.contains(el |+| g.inverse)
  def size: BigInt = grpH.order
}

class LeftCoset[G](g: G, grpH: Grp[G]) {
  override def toString = s"$g |+| ($grpH)"
  implicit def algebra = grpH.algebra
  def contains(el: G) = grpH.contains(g.inverse |+| el)
  def size: BigInt = grpH.order
  def iterator: Iterator[G] = grpH.elements.iterator.map( h => g |+| h )
}

/** Left cosets of G by its subgroup H. */
class LeftCosets[G](grpG: Grp[G], grpH: Grp[G]) {
  override def toString = s"($grpG) / ($grpH)"
  import grpG.{algebra, defaultAction, algorithms}
  def size: BigInt = grpG.order / grpH.order
  def iterator: Iterator[LeftCoset[G]] = {
    val bo = algorithms.baseOrder(grpG.chain.base)(defaultAction)
    def rec(g: G, chainG: Chain[G], subgrpH: Grp[G]): Iterator[LeftCoset[G]] = chainG match {
      case node: Node[G] =>
        for {
          b <- node.orbit.iterator
          bg = defaultAction.actr(b, g)
          (subgrpHnext, transversal) = subgrpH.stabilizer(bg)(defaultAction) if transversal.orbit.min(Order.ordering(bo)) == bg
          nextG = node.u(b) |+| g
          element <- rec(nextG, node.next, subgrpHnext)
        } yield element
      case _: Term[G] =>
        if (subgrpH.order > 1)
          println(subgrpH)
        Iterator(new LeftCoset(g, grpH))
    }
    rec(algebra.id, grpG.chain, grpH)
  }
}
