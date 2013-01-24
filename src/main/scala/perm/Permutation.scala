package com.faacets.perm

import Implicits._

trait Permutation[P] extends GroupElement[P] with Ordered[P] {
  def domainSize: Int
  def image(el: Domain): Domain
  def images: Vector[Domain]
  def compare(that: P): Int
  def toExplicit: ExplicitPermutation = new ExplicitPermutation(images)
}
