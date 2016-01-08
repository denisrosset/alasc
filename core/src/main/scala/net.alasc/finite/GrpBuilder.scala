package net.alasc.finite

import scala.util.Random

import spire.algebra.{Eq, Group}

/** Builder for groups composed of elements of type `G`. */
abstract class GrpBuilder[G] {

  /** Group operations on type `G`. */
  implicit def group: Group[G]

  /** Equality for type `G`. */
  implicit def equ: Eq[G]

  def trivial: Grp[G]

  def fromGenerators(generators: Iterable[G]): Grp[G]

  def fromGeneratorsAndOrder(generators: Iterable[G], order: BigInt): Grp[G]

  def fromGeneratorsRandomElementsAndOrder(generators: Iterable[G], randomElement: Random => G, order: BigInt): Grp[G]

}
