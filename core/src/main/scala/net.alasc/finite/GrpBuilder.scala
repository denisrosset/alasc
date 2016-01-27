package net.alasc.finite

import scala.util.Random

import spire.algebra.{Eq, Group}

/** Builder for groups composed of elements of type `G`. */
abstract class GrpBuilder[G] {

  type GG <: Grp[G]

  /** Group operations on type `G`. */
  implicit def group: Group[G]

  /** Equality for type `G`. */
  implicit def equ: Eq[G]

  def trivial: GG

  def fromGenerators(generators: Iterable[G]): GG

  def fromGeneratorsAndOrder(generators: Iterable[G], order: BigInt): GG

  def fromGeneratorsRandomElementsAndOrder(generators: Iterable[G], randomElement: Random => G, order: BigInt): GG

  def fromGrp(grp: Grp[G]): GG

}
