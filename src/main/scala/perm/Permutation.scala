package com.faacets.perm

import Implicits._

trait Permutation[P] extends Ordered[P] {
  def domainSize: Int
  def images: Vector[Domain]

  def compare(that: P): Int
  def verify: Boolean

  def *(other: P): P
  def inverse: P

  def isIdentity: Boolean
  def identity: P

  def image(el: Domain): Domain
  def hasInSupport(el: Domain): Boolean
  def support: Iterable[Domain]
  def cycle(start: Domain): Iterable[Domain]
  def cycles(includeTrivialCycles: Boolean = false): Iterable[(Domain, Int)]

  def toExplicit: ExplicitPermutation
}
