package net.alasc.gap3

import cyclo.Cyclo

import scalin.immutable.{Mat, Vec}

/** Describes the variable space of an optimization problem. */
case class Vars(names: Seq[String]) {

  def dim: Int = names.size

}