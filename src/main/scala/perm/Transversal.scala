package com.faacets.perm

import Implicits._

trait Transversal extends Orbit {
  def apply(el: Int): Permutation
  def size: Int
}
