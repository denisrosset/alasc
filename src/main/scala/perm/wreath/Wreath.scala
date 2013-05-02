package com.faacets
package perm
package wreath

import scala.util.Random
import scala.reflect.ClassTag

class WreathGroup[A <: FiniteGroup[AE], AE <: FiniteElement[AE] : ClassTag,
  H <: PermGroup[HE], HE <: PermElement[HE]](val a: A, val h: H) extends FiniteGroup[WreathElement[AE, HE]] {
  type Element = WreathElement[AE, HE]
  val k = new BaseGroup[A, AE](a, h.degree)
  def identity = new WreathElement(k.identity, h.identity)
  def generators = 
    k.generators.map(new WreathElement(_, h.identity)) ++
  h.generators.map(new WreathElement(k.identity, _))
  def elements = for {
    he <- h.elements
    ke <- k.elements
  } yield new WreathElement(ke, he)
  def contains(e: Element) = k.contains(e.ke) && h.contains(e.he)
  def order = h.order * k.order
  def random(implicit gen: Random) = new WreathElement(k.random, h.random)
  def compatible(e: Element) = k.compatible(e.ke) && h.compatible(e.he)
}

class WreathElement[AE <: FiniteElement[AE], HE <: PermElement[HE]](val ke: BaseElement[AE], val he: HE) extends FiniteElement[WreathElement[AE, HE]] {
  type Element = WreathElement[AE, HE]
  def compatible(that: Element) = ke.compatible(that.ke) && he.compatible(that.he)
  def *(that: Element) = new WreathElement(ke * (that.ke **! he), he * that.he)
  def equal(that: Element) = ke.equal(that.ke) && he.equal(that.he)
  def inverse = new WreathElement((ke ** he).inverse, he.inverse)
  def isIdentity = ke.isIdentity && he.isIdentity
}
