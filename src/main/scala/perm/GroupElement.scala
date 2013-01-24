package com.faacets.perm

import Implicits._

trait GroupElement[P] {
  def *(other: P): P
  def inverse: P
  def isIdentity: Boolean
  def identity: P
  def equal(that: P): Boolean
  def verify: Boolean
}
