package com.faacets.perm
package wreath

import scala.util.Random

/** Represents the 'inhomogenous' wreath product (a...a_n, h), where h is a permutation group and
  * a_1 ... a_n are finite groups. 
  */
trait InhWreathProductGroup extends FiniteGroup {
  wreath =>

  type Group = InhWreathProductGroup
  type Element = InhWreathProductElement

  type A <: FiniteGroup
  type H <: PermutationGroup

  val h: H
  val aArr: Array[AnyRef]

  val k = new {
    type A = wreath.A
    val aArr = wreath.aArr
    val n = wreath.h.degree
  } with InhBaseGroup

  type K = k.type

  def assertValid { h.assertValid; k.assertValid }

  def identity = InhWreathProductElement(k.identity, h.identity)

  def topGroupGeneratorsIterator =
    for (hgen <- h.generatorsIterator)
    yield InhWreathProductElement(k.identity, hgen)

  def baseGroupGeneratorsIterator =
    for (kgen <- k.generatorsIterator)
    yield InhWreathProductElement(kgen, h.identity)

  def generatorsIterator = topGroupGeneratorsIterator ++ baseGroupGeneratorsIterator

  def contains(e: Element) = h.contains(e.hel) && k.contains(e.kel)

  def order = h.order * k.order

  def elementsIterator = for {
    hel <- h.elementsIterator
    kel <- k.elementsIterator
  } yield InhWreathProductElement(kel, hel)

  def randomElement()(implicit gen: Random = Random) =
    InhWreathProductElement(k.randomElement()(gen), h.randomElement()(gen))

  final case class InhWreathProductElement(val kel: k.Element, val hel: h.Element) extends FiniteGroupElement {
    self: Element =>
    val group = InhWreathProductGroup.this

    def assertValid {
      kel.assertValid
      hel.assertValid
    }

    def *(that: Element) = InhWreathProductElement(kel * (that.kel**(hel.inverse)), hel * that.hel)

    def inverse = InhWreathProductElement((kel ** hel).inverse, hel.inverse)

    def isIdentity = kel.isIdentity && hel.isIdentity
    def equal(that: Element) = kel == that.kel && hel == that.hel
  }
}
