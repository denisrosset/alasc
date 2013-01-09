package com.faacets.perm

import Implicits._

class TrivialTransversal(alpha: Domain, degree: Int) extends Transversal {
  override def size = 1
  override def apply(el: Int) = {
    assert(el == alpha)
    Permutation(degree)
  }
  override def contains(el: Int) = (el == alpha)
  override def iterable = List(alpha)
}
