package com.faacets.perm

import Implicits._

trait Orbit {
  def contains(el: Domain): Boolean
  def iterable: Iterable[Domain]
}
