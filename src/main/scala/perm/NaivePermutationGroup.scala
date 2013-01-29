package com.faacets.perm

import scala.collection.mutable
import scala.collection.immutable

class NaivePermutationGroup[T <: Permutation[T]](G: Seq[T]) extends PermutationGroup[T] {
  val degree = G.head.domainSize
  lazy val _elements = {
    val E = mutable.HashSet.empty[T]
    E += G.head.identity
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
  override def order = _elements.size
  override def contains(perm: T) = _elements.contains(perm)
  /** Verifies that all generators have the same degree, and verify them. */
  override def verify: Boolean = !G.exists(_.domainSize != degree) && !G.exists(!_.verify)
  override def generatingSet = G
  def iterator: Iterator[T] = _elements.iterator
}
