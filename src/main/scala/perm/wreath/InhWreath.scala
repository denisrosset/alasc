package com.faacets
package perm
package wreath

import scala.util.Random
import scala.reflect.ClassTag

class InhWreathGroup[A <: FiniteGroup[AE], AE <: FiniteElement[AE] : ClassTag,
  H <: PermGroup[HE], HE <: PermElement[HE]](val a: Array[A], val h: H) extends FiniteGroup[InhWreathElement[AE, HE]] {
  type Element = InhWreathElement[AE, HE]
  val k = new InhBaseGroup[A, AE](a, h.degree)
  def identity = new InhWreathElement(k.identity, h.identity)
  override def toString = a.mkString("(",",",")") + " wr " + h.toString
  def generators = 
    k.generators.map(new InhWreathElement(_, h.identity)) ++
  h.generators.map(new InhWreathElement(k.identity, _))
  def elements = for {
    he <- h.elements
    ke <- k.elements
  } yield new InhWreathElement(ke, he)
  def contains(e: Element) = k.contains(e.ke) && h.contains(e.he)
  def order = h.order * k.order
  def random(implicit gen: Random) = new InhWreathElement(k.random, h.random)
  def compatible(e: Element) = k.compatible(e.ke) && h.compatible(e.he)
}

class InhWreathElement[AE <: FiniteElement[AE], HE <: PermElement[HE]](val ke: InhBaseElement[AE], val he: HE) extends FiniteElement[InhWreathElement[AE, HE]] {
  type Element = InhWreathElement[AE, HE]
  def compatible(that: Element) = ke.compatible(that.ke) && he.compatible(that.he)
  def *(that: Element) = new InhWreathElement(ke * (that.ke **! he), he * that.he)
  def equal(that: Element) = ke.equal(that.ke) && he.equal(that.he)
  def inverse = new InhWreathElement((ke ** he).inverse, he.inverse)
  def isIdentity = ke.isIdentity && he.isIdentity
}
