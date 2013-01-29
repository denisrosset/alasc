package com.faacets.perm

trait PermutationGroup[P <: Permutation[P]] extends Iterable[P] {
  def degree: Int
  def verify: Boolean
  def generatingSet: Iterable[P]
  def order: Int
  def contains(perm: P): Boolean

  def isBase(base: Base) = generatingSet.exists(g => !base.exists(beta => beta != g.image(beta)))
}
