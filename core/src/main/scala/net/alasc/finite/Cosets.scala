package net.alasc.finite

import spire.algebra.Group

import spire.math.SafeLong

trait Cosets[G, Subgrp <: Grp[G] with Singleton] {

  val grp: Grp[G]

  val subgrp: Subgrp

  def size: SafeLong = grp.order / subgrp.order

  def inverse: Cosets[G, Subgrp]

  def iterator: Iterator[Coset[G, Subgrp]]

  def representativesIterator: Iterator[G] = iterator.map(_.representative) // TODO: optimize in implementations

}

trait LeftCosets[G, Subgrp <: Grp[G] with Singleton] extends Cosets[G, Subgrp] {

  override def toString = s"($grp) / ($subgrp)"

  def iterator: Iterator[LeftCoset[G, Subgrp]]

  def inverse: RightCosets[G, Subgrp]

}

abstract class LeftCosetsImpl[G:Group, Subgrp <: Grp[G] with Singleton] extends LeftCosets[G, Subgrp] { lhs =>

  def inverse: RightCosets[G, Subgrp] = new RightCosets[G, Subgrp] {

    val grp = lhs.grp

    val subgrp = lhs.subgrp

    def inverse = lhs

    def iterator = lhs.iterator.map(_.inverse)

  }

}

trait RightCosets[G, Subgrp <: Grp[G] with Singleton] extends Cosets[G, Subgrp] {

  override def toString = s"($subgrp) \\ ($grp)"

  def inverse: LeftCosets[G, Subgrp]

  def iterator: Iterator[RightCoset[G, Subgrp]]

}
