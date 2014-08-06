package net.alasc.math
package bsgs

import scala.util.Random
import spire.syntax.groupAction._
import spire.syntax.group._
import net.alasc.algebra.Permutation

abstract class Transversal[P] extends ReadOnlyMap[Int, InversePair[P]] {
  def builder: TransversalBuilder
  implicit def algebra: Permutation[P]

  def beta: Int
  /** Updates the transversal with `newGenerators`, with `allGenerators` containing
    * both the new and old generators.
    */
  def updated(newGenerators: Iterable[P], allGenerators: Iterable[P]): Transversal[P]
  /** Conjugates the transversal by an element `p`. */
  def conjugatedBy(p: P): Transversal[P] = conjugatedBy(InversePair(p, p.inverse))
  /** Conjugates the transversal by an element `p`. Its inverse `pinv` is provided for speed. */
  def conjugatedBy(ip: InversePair[P]): Transversal[P]
  /** Changes the type of the transversal values. Both p: P and q: Q = f(p) must have the same
    * action on integers. */
  def mapValues[Q: Permutation](f: P => Q): Transversal[Q]
  /** Returns a random element of the transversal. */
  def random(rand: Random): InversePair[P] = valuesIterator.drop(rand.nextInt(size)).next
  /** Returns the orbit set. */
  def orbitSet: Set[Int] = keysIterator.toSet
  /** Checks the sanity of the transversal. */
  def check: Unit = {
    for (b <- keysIterator)
      assert(beta <|+| apply(b).g == b && b <|+| apply(b).gInv == beta)
  }
}

abstract class TransversalBuilder {
  def empty[P](beta: Int)(implicit algebra: Permutation[P]): Transversal[P]
}
