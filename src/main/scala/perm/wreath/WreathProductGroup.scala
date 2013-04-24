package com.faacets.perm
package wreath

import scala.util.Random

/** Represents the wreath product a Wr h, where h is a permutation group and
  * a a finite group.
  */
trait WreathProductGroup extends FiniteGroup {
  wreath =>
  type A <: FiniteGroup
  type H <: PermutationGroup
  val a: A
  val h: H
  type Group = WreathProductGroup
  type Element = WreathProductElement

  val k = new {
    type A = wreath.A
    val a: A = wreath.a
    val n = wreath.h.degree
  } with BaseGroup

  type K = k.type

  def assertValid { h.assertValid; k.assertValid }

  def identity = WreathProductElement(k.identity, h.identity)

  def topGroupGeneratorsIterator =
    for (hgen <- h.generatorsIterator)
    yield WreathProductElement(k.identity, hgen)

  def baseGroupGeneratorsIterator =
    for (kgen <- k.generatorsIterator)
    yield WreathProductElement(kgen, h.identity)

  def generatorsIterator = topGroupGeneratorsIterator ++ baseGroupGeneratorsIterator

  def contains(e: Element) = h.contains(e.hel) && k.contains(e.kel)

  def order = h.order * k.order

  def elementsIterator = for {
    hel <- h.elementsIterator
    kel <- k.elementsIterator
  } yield WreathProductElement(kel, hel)

  def randomElement()(implicit gen: Random = Random) =
    WreathProductElement(k.randomElement()(gen), h.randomElement()(gen))

  final case class WreathProductElement(val kel: k.Element, val hel: h.Element) extends FiniteGroupElement {
    self: Element =>
    val group = WreathProductGroup.this

    def assertValid {
      kel.assertValid
      hel.assertValid
    }

    def *(that: Element) = WreathProductElement(kel * (that.kel**(hel.inverse)), hel * that.hel)

    def inverse = WreathProductElement((kel ** hel).inverse, hel.inverse)

    def isIdentity = kel.isIdentity && hel.isIdentity
    def equal(that: Element) = kel == that.kel && hel == that.hel
  }
}
