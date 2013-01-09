package com.faacets.perm

import Implicits._
import scala.collection.mutable
import scala.collection.immutable

class OrbitSet(set: immutable.SortedSet[Domain]) extends Orbit {
  val orbit = set
  override def contains(el: Domain) = orbit.contains(el)
  override def iterable = orbit
}

object OrbitSet {
  def fromGenerators(el: Domain, G: Iterable[Permutation]) = {
    val Delta = scala.collection.mutable.HashSet.empty[Int]
    def visit(a: Int) {
      if (!Delta(a)) {
        Delta += a
        for (g <- G) visit(a**g)
      }
    }
    visit(el)
    new OrbitSet(scala.collection.immutable.SortedSet.empty[Int] ++ Delta)
  }
}
