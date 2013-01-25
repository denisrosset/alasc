package com.faacets.perm

trait Transversal[T <: Permutation[T]] {
  def contains(el: Domain): Boolean
  def iterable: Iterable[Domain]
  def apply(el: Int): T
  def size: Int
}
