package net.alasc.finite

import spire.math.SafeLong

trait Cosets[G] {

  val grp: Grp[G]

  val subgrp: Grp[G]

  def size: SafeLong

}

trait LeftCosets[G] extends Cosets[G] {

  override def toString = s"($grp) / ($subgrp)"

  def iterator: Iterator[LeftCoset[G]]

  def size: SafeLong = grp.order / subgrp.order

}

trait RightCosets[G] extends Cosets[G] {

  override def toString = s"($subgrp) \ ($grp)"

  def iterator: Iterator[RightCoset[G]]

  def size: SafeLong = grp.order / subgrp.order

}
