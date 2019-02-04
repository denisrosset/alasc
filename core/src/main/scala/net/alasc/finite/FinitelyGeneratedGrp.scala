package net.alasc.finite

import net.alasc.attributes.Attributable
import spire.algebra.{Eq, Group}

abstract class AbstractGrp extends Attributable {

  /** Element type */
  type G

  /** Group operations on type `G`. */
  implicit def group: Group[G]

  /** Equality for type `G`. */
  implicit def equ: Eq[G]

}

object AbstractGrp {

  type Aux[G0] = AbstractGrp { type G = G0 }

}

abstract class FinitelyGeneratedGrp extends AbstractGrp {

  /** Generators of the group, does not contain the identity. */
  def generators: Seq[G]

  /** Number of group generators. */
  def nGenerators: Int

  /** Returns the i-th generator. */
  def generator(i: Int): G

  /** Returns whether this is the trivial group with a single identity element. */
  def isTrivial: Boolean = generators.isEmpty

}

object FinitelyGeneratedGrp {

  type Aux[G0] = FinitelyGeneratedGrp { type G = G0 }

}
