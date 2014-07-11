package net.alasc

import scala.util.Random

/** PRepr: represents a finite group by its action on a permutation domain. */
trait PRepr[F <: Finite[F]] extends Action[Int, F] {
  /** Is the action faithful, i.e. two different group elements always have the same action. */
  def faithful: Boolean
  /** Dimension/degree of the permutation representation. */
  def dimension: Int
  /** Identity element of the group represented by the action. */ 
  def identity: F
  /** Iterable over the domain of the permutation representation. */
  def domain: Iterable[Dom]
  /** Compute the image of the domain element k under the action of an element f. */
  def apply(f: F, k: Dom): Dom
  /** Computes the permutation corresponding to the element f. */
  def toPerm(f: F): Perm
  /** Flag: implementation have to override hashCode and equals. */
  def dontForgetToOverrideHashCodeAndEquals: Boolean
  def actr(v: Int, f: F): Int = apply(f, Dom._0(v))._0
}

trait PReprImpl[F <: Finite[F]] extends PRepr[F] {
  def domain: Iterable[Dom] = Dom.domain(dimension)
  def toPerm(f: F): Perm = Perm.fromImages(dimension)(k => apply(f, k))
}

/** Trivial action for finite elements that are permutations themselves. */
case class TrivialPRepr[P <: Permuting[P]](val identity: P) extends PRepr[P] with PReprImpl[P] {
  def dontForgetToOverrideHashCodeAndEquals = true // by the case class
  def faithful = true
  def dimension = identity.size
  def apply(p: P, k: Dom) = p.image(k)
}
