package net.alasc

import scala.util.Random

/** Action: represents a finite group by its action on a permutation domain. */
trait Action[F <: GenFinite] {
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
}

trait ActionLike[F <: GenFinite] extends Action[F] {
  def domain: Iterable[Dom] = Dom.domain(dimension)
  def apply(f: F, k: Dom): Dom
  def toPerm(f: F): Perm = Perm.fromImages(dimension)(k => apply(f, k))
}

/** Trivial action for finite elements that are permutations themselves. */
case class TrivialAction[P <: Permuting[P]](val identity: P) extends Action[P] with ActionLike[P] {
  def faithful = true
  def dimension = identity.size
  def apply(p: P, k: Dom) = p.image(k)
}
