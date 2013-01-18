package com.faacets.perm

import Implicits._

trait PermutationGroup[P <: Permutation[P]] {
  def degree: Int
  def verify: Boolean
  def elements: Iterable[P]
  def generatingSet: Iterable[P]
  def order: Int
  def isBase(base: Base): Boolean
  def contains(perm: P): Boolean
}
