package net.alasc
package wreath

import scala.util.Random
import scala.reflect.ClassTag

case class InhWreathGroup[A <: FiniteGroup[AE], AE <: FiniteElement[AE] : ClassTag, H <: PermGroup[HE], HE <: PermElement[HE]](a: Array[A], h: H) extends InhWreathGroupTrait[InhWreathElement[AE, HE], A, AE, H, HE] {
  def make(ke: InhBaseElement[AE], he: HE) = InhWreathElement(ke, he)
}

abstract class InhWreathGroupTrait[IWE <: InhWreathElementTrait[IWE, AE, HE], A <: FiniteGroup[AE], AE <: FiniteElement[AE] : ClassTag,
  H <: PermGroup[HE], HE <: PermElement[HE]] extends FiniteGroup[IWE] {
  val a: Array[A]
  val h: H
  val k = new InhBaseGroup[A, AE](a, h.degree)
  def make(ke: InhBaseElement[AE], he: HE): IWE
  def identity = make(k.identity, h.identity)
//  def toTeX = k.toTeX + TeX(" \\rtimes ") + h.toTeX
  def kGenerators = k.generators.map(make(_, h.identity))
  def hGenerators = h.generators.map(make(k.identity, _))
  def generators = kGenerators ++ hGenerators
  
  def elements = for {
    he <- h.elements
    ke <- k.elements
  } yield make(ke, he)
  def contains(e: IWE) = k.contains(e.ke) && h.contains(e.he)
  def order = h.order * k.order
  def random(implicit gen: Random) = make(k.random, h.random)
  def compatible(e: IWE) = k.compatible(e.ke) && h.compatible(e.he)
}

case class InhWreathElement[AE <: FiniteElement[AE], HE <: PermElement[HE]](ke: InhBaseElement[AE], he: HE) extends InhWreathElementTrait[InhWreathElement[AE, HE], AE, HE] {
  def make(newKe: InhBaseElement[AE], newHe: HE) = InhWreathElement(newKe, newHe)
}

trait InhWreathElementTrait[IWE <: InhWreathElementTrait[IWE, AE, HE], AE <: FiniteElement[AE], HE <: PermElement[HE]] extends FiniteElement[IWE] {
//  def toTeX = TeX("\\big (") + ke.toTeX + TeX(",") + he.toTeX + TeX("\\big )")
  val ke: InhBaseElement[AE]
  val he: HE
  def compatible(that: IWE) = ke.compatible(that.ke) && he.compatible(that.he)
  def make(newKe: InhBaseElement[AE], newHe: HE): IWE
  def *(that: IWE) = make(ke * (that.ke **! he), he * that.he)
  override def hashCode() = ke.hashCode() + he.hashCode()*41
  def ===(that: IWE) = ke === that.ke && he === that.he
  def inverse = make((ke ** he).inverse, he.inverse)
  def isIdentity = ke.isIdentity && he.isIdentity
  def basePart = make(ke, he*he.inverse)
  def nonBasePart = make(ke*ke.inverse, he)
}
