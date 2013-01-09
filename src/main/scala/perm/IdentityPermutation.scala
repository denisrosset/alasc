package com.faacets.perm

import Implicits._

class IdentityPermutation(size: Int) extends Permutation {
  override def image(el: Domain) = el
  override def cycle(el: Domain) = List(el)
  override def domainSize = size
  override def inverse = this
  override def *(other: Permutation): Permutation = other
  override def support = List()
  override def hasInSupport(el: Domain) = false
  override def isIdentity = true
  override def cycles(includeTrivial: Boolean): Iterable[(Domain, Int)] = {
    if (includeTrivial)
      (0 until size).map( (_, 1) )
    else
      List()
  }
  override def verify = size > 0
  override def resizedTo(newSize: Int) = if (size == newSize) Some(this) else Some(new IdentityPermutation(newSize))
}
