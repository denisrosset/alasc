package net.alasc.finite

import spire.algebra.{Eq, Group, LeftAction, RightAction}
import spire.math.SafeLong
import spire.syntax.group._

trait Coset[G, Subgrp <: Grp[G] with Singleton] {

  override def hashCode = sys.error("Not implemented")

  val subgrp: Subgrp

  /** Number of elements in this coset. */
  def size: SafeLong

  /** Iterator through the coset elements. */
  def iterator: Iterator[G]

  /** Checks whether this coset contains the given element. */
  def contains(el: G): Boolean

  def representative: G

}

class RightCoset[G, Subgrp <: Grp[G] with Singleton](val representative: G, val subgrp: Subgrp) extends Coset[G, Subgrp] { lhs =>

  import subgrp.group

  override def equals(that: Any) = that match {
    case rhs: RightCoset[G, _] if subgrp eq rhs.subgrp => // the type G is attested by the equality of subgroups
      import subgrp.group
      subgrp.contains(lhs.representative |+| rhs.representative.inverse)
    case _ => false
  }

  override def toString = s"($subgrp) |+| $representative"

  def contains(el: G) = subgrp.contains(el |+| representative.inverse)

  def size: SafeLong = subgrp.order

  def iterator: Iterator[G] = subgrp.iterator.map( h => h |+| representative )

  def inverse: LeftCoset[G, Subgrp] = new LeftCoset(representative.inverse, subgrp)

}

object RightCoset {

  /** Equivalence relation on cosets. */
  implicit def equ[G:Group, Subgrp <: Grp[G] with Singleton]: Eq[RightCoset[G, Subgrp]] =
    new Eq[RightCoset[G, Subgrp]] {
      def eqv(lhs: RightCoset[G, Subgrp], rhs: RightCoset[G, Subgrp]) =
        lhs.subgrp.contains(lhs.representative |+| rhs.representative.inverse)
    }

  implicit def action[G:Group, Subgrp <: Grp[G] with Singleton]: RightAction[RightCoset[G, Subgrp], G] =
    new RightAction[RightCoset[G, Subgrp], G] {
      def actr(lhs: RightCoset[G, Subgrp], rhs: G) = new RightCoset[G, Subgrp](lhs.representative |+| rhs, lhs.subgrp)
    }

}

class LeftCoset[G, Subgrp <: Grp[G] with Singleton](val representative: G, val subgrp: Subgrp) extends Coset[G, Subgrp] { lhs =>

  import subgrp.group

  override def equals(that: Any) = that match {
    case rhs: LeftCoset[G, _] if subgrp eq rhs.subgrp => // the type G is attested by the equality of subgroups
      import subgrp.group
      subgrp.contains(lhs.representative.inverse |+| rhs.representative)
    case _ => false
  }

  override def toString = s"$representative |+| ($subgrp)"

  def contains(el: G) = subgrp.contains(representative.inverse |+| el)

  def size: SafeLong = subgrp.order

  def iterator: Iterator[G] = subgrp.iterator.map( h => representative |+| h )

  def inverse: RightCoset[G, Subgrp] = new RightCoset(representative.inverse, subgrp)

}

object LeftCoset {

  /** Equivalence relation on cosets. */
  implicit def equ[G:Group, Subgrp <: Grp[G] with Singleton]: Eq[LeftCoset[G, Subgrp]] =
    new Eq[LeftCoset[G, Subgrp]] {
      def eqv(lhs: LeftCoset[G, Subgrp], rhs: LeftCoset[G, Subgrp]) =
        lhs.subgrp.contains(lhs.representative.inverse |+| rhs.representative)
    }

  implicit def action[G:Group, Subgrp <: Grp[G] with Singleton]: LeftAction[LeftCoset[G, Subgrp], G] =
    new LeftAction[LeftCoset[G, Subgrp], G] {
      def actl(lhs: G, rhs: LeftCoset[G, Subgrp]) = new LeftCoset[G, Subgrp](lhs |+| rhs.representative, rhs.subgrp)
    }

}
