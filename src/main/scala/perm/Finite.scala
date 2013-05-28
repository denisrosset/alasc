package com.faacets
package perm

import scala.util.Random
import scala.reflect.ClassTag

trait FiniteElementLike extends HasTeX {
  def isIdentity: Boolean
}

trait FiniteElement[E <: FiniteElement[E]] extends FiniteElementLike {
  def compatible(that: E): Boolean
  def inverse: E
  def *(that: E): E
  def ===(that: E): Boolean
  override def equals(that: Any) = if (this.getClass() == that.getClass()) this === that.asInstanceOf[E] else false
}

trait FiniteGroup[E <: FiniteElement[E]] extends HasTeX {
  /** Tests whether element e is compatible with group structure. */
  def compatible(e: E): Boolean
  /** Tests if e is contained in this group. */
  def contains(e: E): Boolean
  /** Iterates through the group elements. */
  def elements: Iterator[E]
  /** Iterates through the group generators. */
  def generators: Iterator[E]
  /** Identity element of this group. */
  def identity: E
  /** Order of this group. */
  def order: BigInt
  /** Generates a random group element.
    * 
    * @param gen Instance of random generator to use.
    */
  def random(implicit gen: Random): E

  def directCopies(n: Int)(implicit c: ClassTag[E]) = new wreath.BaseGroup[FiniteGroup[E], E](this, n)
  def elementClassTag: ClassTag[E] = ClassTag[E](identity.getClass)
}
