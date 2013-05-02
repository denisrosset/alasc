package com.faacets
package perm

import scala.util.Random

trait Action[SE <: FiniteElement[SE], DE <: PermElement[DE]] extends Function1[SE, DE] {
  def dim: Int
}

case class TrivialAction[E <: PermElement[E]](val dim: Int) extends Action[E, E] {
  def apply(e: E) = e
}

case class ActionGroup[A <: Action[F, P], 
  G <: FiniteGroup[F],
  F <: FiniteElement[F],
  P <: PermElement[P]](g: G, a: A) extends PermGroup[ActionElement[A, F, P]] {
  type Element = ActionElement[A, F, P]
  def degree = a(g.identity).size
  def compatible(e: Element) = g.compatible(e.f)
  def contains(e: Element) = g.contains(e.f)
  def elements = g.elements.map(ActionElement(_, a))
  def generators = g.elements.map(ActionElement(_, a))
  def identity = ActionElement(g.identity, a)
  def order = g.order
  def random(implicit gen: Random) = ActionElement(g.random, a)
  def fromExplicit(p: Perm) = elements.find(_.explicit.equal(p))
}

case class ActionElement[A <: Action[F, P], F <: FiniteElement[F], P <: PermElement[P]](f: F, a: A) extends PermElement[ActionElement[A, F, P]] {
  type Element = ActionElement[A, F, P]
  def compatible(that: Element) = a == that.a
  def *(that: Element) = {
    require_(compatible(that))
    ActionElement(f*that.f, a)
  }
  def equal(that: Element) = {
    require_(compatible(that))
    f.equal(that.f)
  }
  def inverse = ActionElement(f.inverse, a)
  def isIdentity = f.isIdentity
  def compare(that: Element) = {
    require_(compatible(that))
    val firstNotEqual = domain.find(k => image(k) != that.image(k))
    firstNotEqual match {
      case None => 0
      case Some(k) if image(k) <= that.image(k) => -1
      case _ => 1
    }
  }
  def explicit = new Perm(images0)
  def image(k: Domain) = a(f).image(k)
  def images = a(f).images
  def images0 = a(f).images0
  def size = a.dim
}
