package com.faacets
package perm

import scala.util.Random

trait Action[G <: FiniteGroup[E], E <: FiniteElement[E]] {
  val g: G
  def imageOf(e: E, k: Domain): Domain
  def imagesOf0(e: E): ArrayDomain0
  def imagesOf(e: E): ArrayDomain1
  val dim: Int
}

case class TrivialAction[G <: PermGroup[E], E <: PermElement[E]](g: G) extends Action[G, E] {
  val dim = g.degree
  def imageOf(e: E, k: Domain) = e.image(k)
  def imagesOf0(e: E) = e.images0
  def imagesOf(e: E) = e.images
}

case class ActionGroup[A <: Action[G, F], G <: FiniteGroup[F], F <: FiniteElement[F]](a: A) extends PermGroup[ActionElement[A, F]] {
  type Element = ActionElement[A, F]
  def degree = a.dim
  def compatible(e: Element) = a.g.compatible(e.f)
  def contains(e: Element) = a.g.contains(e.f)
  def elements = a.g.elements.map(ActionElement(_, a))
  def generators = a.g.elements.map(ActionElement(_, a))
  def identity = ActionElement(a.g.identity, a)
  def order = a.g.order
  def random(implicit gen: Random) = ActionElement(a.g.random, a)
}

case class ActionElement[A <: Action[_, F], F <: FiniteElement[F]](f: F, a: A) extends PermElement[ActionElement[A, F]] {
  type Element = ActionElement[A, F]
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
  def image(k: Domain) = a.imageOf(f, k)
  def images = a.imagesOf(f)
  def images0 = a.imagesOf0(f)
  def size = a.dim
}
