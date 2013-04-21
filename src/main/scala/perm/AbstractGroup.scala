package com.faacets.perm

trait AbstractGroup {
  import scala.util.Random

  type Element <: AbstractElement /** Type of this group element. */
  type Group <: AbstractGroup /** Type of this group. */
  type Predicate = (Element => Boolean) /** Predicate defining a subgroup. */

  def identity: Element /** Identity element of this group. */
  def randomElement()(implicit gen: Random = Random): Element /** Produces a random element. */
  def assertValid /** Asserts that this group is consistent. */
  trait AbstractElement {
    val group: Group /** Group this element is member of. */
    def *(that: Element): Element /** Multiplication by another group element. */
    def inverse: Element /** Inverse of this group element. */
    def isIdentity: Boolean /** Checks if this group element is the identity. */
    def equal(that: Element): Boolean  /** Check if this group element is equal to another group element. */
    def assertValid /** Asserts that this group element is consistent. */
  }
}
