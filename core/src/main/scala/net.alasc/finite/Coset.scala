package net.alasc.finite

import spire.syntax.group._

trait Coset[G] {

  def subgrp: Grp[G]

  /** Number of elements in this coset. */
  def size: BigInt

  /** Iterator through the coset elements. */
  def iterator: Iterator[G]

  /** Checks whether this coset contains the given element. */
  def contains(el: G): Boolean

}

class RightCoset[G](val g: G, val subgrp: Grp[G]) extends Coset[G] {

  import subgrp.group

  override def toString = s"($subgrp) |+| $g"

  def contains(el: G) = subgrp.contains(el |+| g.inverse)

  def size: BigInt = subgrp.order

  def iterator: Iterator[G] = subgrp.iterator.map( h => h |+| g )

  def inverse: LeftCoset[G] = new LeftCoset(g.inverse, subgrp)

}

class LeftCoset[G](val g: G, val subgrp: Grp[G]) extends Coset[G] {

  import subgrp.group

  override def toString = s"$g |+| ($subgrp)"

  def contains(el: G) = subgrp.contains(g.inverse |+| el)

  def size: BigInt = subgrp.order

  def iterator: Iterator[G] = subgrp.iterator.map( h => g |+| h )

  def inverse: RightCoset[G] = new RightCoset(g.inverse, subgrp)

}
