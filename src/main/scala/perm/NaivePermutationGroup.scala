package com.faacets.perm

import scala.collection.mutable
import scala.collection.immutable

/** Implementation of a permutation group that constructs explicitly all elements.
  * 
  * Very slow, but easy to implement. Can be used to check the validity of other approaches.
  * 
  * @param id Identity element of the group.
  * @param G  Generators of the group.
  * 
  */
class NaivePermutationGroup[T <: Permutation[T]](id: T, G: Seq[T]) extends PermutationGroup[T] {
  def degree = id.domainSize
  def identity = id
  lazy val _elements = {
    val E = mutable.HashSet.empty[T]
    E += identity
    def tryToAddOne: Boolean = {
      for (e <- E.toList; g <- G) {
        if (!E.contains(g*e)) {
          E += g*e
          return true
        }
      }
      return false
    }
    while (tryToAddOne) { }
    Set() ++ E
  }
  override def toString = G.mkString("Naive permutation group with generators:", ", ", ".")
  def order = _elements.size
  def contains(perm: T) = _elements.contains(perm)
  /** Verifies that all generators have the same degree, and verify them. */
  def assertValid {
    assert(G.forall(_.domainSize == degree))
    G.foreach ( _.assertValid )
  }
  def generators = G
  def iterator: Iterator[T] = _elements.iterator
  def randomElement = iterator.drop(scala.util.Random.nextInt(order.intValue)).next()
}
