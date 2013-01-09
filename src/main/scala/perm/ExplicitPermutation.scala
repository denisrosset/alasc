package com.faacets.perm

import Implicits._
import scala.annotation.tailrec

import scala.collection.mutable
import scala.collection.immutable

class ExplicitPermutation(P: Vector[Domain]) extends Permutation {
  override def image(el: Domain) = P(el)
  override def images = P
}
