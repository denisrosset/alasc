package com.faacets.perm

import Implicits._
import scala.collection.mutable
import scala.collection.immutable

class NaivePermutationGroup(G: Seq[Permutation]) extends PermutationGroup {
  val degree = G.head.domainSize

  override def toString = G.mkString("NaivePermutationGroup(", ", ", ")")

  /** Verifies that all generators have the same degree, and verify them. */
  def verify: Boolean = !G.exists(_.domainSize != degree) && !G.exists(!_.verify)

  override def isBase(base: Base) = G.exists(g => !base.exists(beta => beta != g.image(beta)))

  override def elements: Iterable[Permutation] = {
    val E = mutable.HashSet.empty[Permutation]
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
    return E.toList
  }
}
