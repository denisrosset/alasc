package net.alasc.finite

import spire.syntax.group._

trait Cosets[G] {

  type GG <: Grp[G] with Singleton

  val grp: GG

  val subgrp: Grp.SubgroupOf[GG, G]

}

trait LeftCosets[G] extends Cosets[G] {

  override def toString = s"($grp) / ($subgrp)"

  def iterator: Iterator[LeftCoset[G]]

  def size: BigInt = grp.order / subgrp.order

}


trait RightCosets[G] extends Cosets[G] {

  override def toString = s"($subgrp) \ ($grp)"

  def iterator: Iterator[RightCoset[G]]

  def size: BigInt = grp.order / subgrp.order

}
