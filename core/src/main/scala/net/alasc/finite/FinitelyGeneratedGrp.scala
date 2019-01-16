package net.alasc.finite

import net.alasc.attributes.Attributable
import spire.algebra.{Eq, Group}

abstract class FinitelyGeneratedGrp[G] extends Attributable {

  /** Group operations on type `G`. */
  implicit def group: Group[G]

  /** Equality for type `G`. */
  implicit def equ: Eq[G]

  /** Generators of the group, does not contain the identity. */
  def generators: Seq[G]

  /** Number of group generators. */
  def nGenerators: Int

  /** Returns the i-th generator. */
  def generator(i: Int): G

  /** Returns whether this is the trivial group with a single identity element. */
  def isTrivial: Boolean = generators.isEmpty

}
