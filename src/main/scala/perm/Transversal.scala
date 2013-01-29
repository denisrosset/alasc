package com.faacets.perm

trait Transversal[T <: Permutation[T]] {
  def contains(el: Domain): Boolean
  def orbitIterator: Iterator[Domain]
  def elementsIterator: Iterator[T]
  def apply(el: Int): T
  def size: Int
}
