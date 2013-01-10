package com.faacets.perm

import Implicits._

trait PermutationGroup {
  def degree: Int
  def elements: Iterable[Permutation]
  def generatingSet = elements
  def isBase(base: Base): Boolean = elements.exists(g => !base.exists(beta => beta != g.image(beta)))
  def orbit(el: Domain): Orbit = OrbitSet.fromGenerators(el, generatingSet)
  def contains(P: Permutation): Boolean
}

object PermutationGroup {
  def apply(args: Permutation*): PermutationGroup = {
    val degree = args.map(_.domainSize).max
    new NaivePermutationGroup(args.filter(!_.isIdentity).map(_.resizedTo(degree).get))
  }
}
