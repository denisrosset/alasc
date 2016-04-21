package net.alasc.finite

import spire.algebra.Group

import spire.math.SafeLong

trait Cosets[G] {

  val grp: Grp[G]

  val subgrp: Grp[G]

  def size: SafeLong = grp.order / subgrp.order

  def inverse: Cosets[G]

}

trait LeftCosets[G] extends Cosets[G] {

  override def toString = s"($grp) / ($subgrp)"

  def iterator: Iterator[LeftCoset[G]]

  def inverse: RightCosets[G]

}

abstract class LeftCosetsImpl[G:Group] extends LeftCosets[G] { lhs =>

  def inverse: RightCosets[G] = new RightCosets[G] {

    val grp = lhs.grp

    val subgrp = lhs.subgrp

    def iterator = lhs.iterator.map(_.inverse)

    def inverse = lhs

  }

}

trait RightCosets[G] extends Cosets[G] {

  override def toString = s"($subgrp) \\ ($grp)"

  def iterator: Iterator[RightCoset[G]]

  def inverse: LeftCosets[G]

}
