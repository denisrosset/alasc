package com.faacets.perm

/** Generic trait for a black-box group element.
  */
trait GroupElement[P] {
  def *(other: P): P /** Multiplication by another group element. */
  def inverse: P /** Inverse of this group element. */
  def isIdentity: Boolean /** Checks if this group element is the identity. */
  def identity: P /** Returns the identity group element, i.e. this * this.inverse */
  def equal(that: P): Boolean /** Check if this group element is equal to another group element. */
  def verify: Boolean /** Check this group element for consistency. */
}
