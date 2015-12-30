package net.alasc.math

import spire.algebra.{Eq, Group, Order}
import spire.syntax.group._
import spire.syntax.action._

import net.alasc.algebra._

import bsgs._

class RightCoset[G](val grpH: Grp[G], val g: G) {
  import grpH.{group, equ}
  def contains(el: G) = grpH.contains(el |+| g.inverse)
  def size: BigInt = grpH.order
  def leftCoset: LeftCoset[G] = new LeftCoset(g.inverse, grpH)
}

class LeftCoset[G](val g: G, val grpH: Grp[G]) {
  import grpH.{group, equ}
  override def toString = s"$g |+| ($grpH)"
  def contains(el: G) = grpH.contains(g.inverse |+| el)
  def size: BigInt = grpH.order
  def iterator: Iterator[G] = grpH.iterator.map( h => g |+| h )
  def rightCoset: RightCoset[G] = new RightCoset(grpH, g.inverse)
}

/** Left cosets of G by its subgroup H. */
class LeftCosets[G](grpG: Grp[G], grpH: Grp[G]) {
  override def toString = s"($grpG) / ($grpH)"
  import grpG.{representation, group, algorithms}
  def size: BigInt = grpG.order / grpH.order
  def iterator: Iterator[LeftCoset[G]] = {
    val bo = bsgs.algorithms.BaseOrder(grpG.chain.base)(representation.action)
    def rec(g: G, chainG: Chain[G], subgrpH: Grp[G]): Iterator[LeftCoset[G]] = chainG match {
      case node: Node[G] =>
        for {
          b <- node.orbit.iterator
          bg = representation.action.actr(b, g)
          (subgrpHnext, transversal) = subgrpH.stabilizer(bg, representation) if transversal.orbit.min(Order.ordering(bo)) == bg
          nextG = node.u(b) |+| g
          element <- rec(nextG, node.next, subgrpHnext)
        } yield element
      case _: Term[G] =>
        assert(subgrpH.order == 1)
        Iterator(new LeftCoset(g, grpH))
    }
    rec(Group[G].id, grpG.chain, grpH)
  }
}

class RightCosets[G](grpH: Grp[G], grpG: Grp[G]) {
  override def toString = s"($grpH) \\ ($grpG)"
  def size: BigInt = grpG.order / grpH.order
  def iterator: Iterator[RightCoset[G]] = new LeftCosets(grpG, grpH).iterator.map(_.rightCoset)
}
