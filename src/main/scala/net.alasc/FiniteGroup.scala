package net.alasc

import scala.util.Random

trait GenFiniteGroup extends Any {
  /** Iterates through the group elements. */
  def elements: Iterable[GenFinite]
  /** Sequence of the group generators. */
  def generators: Seq[GenFinite]
  /** Identity element of this group. */
  def identity: GenFinite
  /** Order of this group. */
  def order: BigInt
  /** Generates a random group element.
    * 
    * @param gen Instance of random generator to use.
    */
  def random(implicit gen: Random = Random): GenFinite
}

trait FiniteGroup[F <: Finite[F]] extends GenFiniteGroup {
  /** Tests if an element is contained in this group. */
  def contains(f: F): Boolean
  /** Iterates through the group elements. */
  def elements: Iterable[F]
  /** Sequence of the group generators. */
  def generators: Seq[F]
  /** Identity element of this group. */
  def identity: F
  /** Order of this group. */
  def random(implicit gen: Random = Random): F
}

trait GenFiniteGroupLike extends GenFiniteGroup

trait FiniteGroupLike[F <: Finite[F]] extends FiniteGroup[F]
