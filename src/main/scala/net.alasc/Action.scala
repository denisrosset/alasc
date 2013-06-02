package net.alasc

import scala.util.Random

trait Action[-SE <: FiniteElementLike, DE <: PermElement[DE]] extends Function1[SE, DE] {
}

case class TrivialAction[E <: PermElement[E]]() extends Action[E, E] {
  override def hashCode = 0xcafebabe
  override def equals(that: Any) = that match {
    case that1: TrivialAction[_] => true
    case _ => false
  }
  def apply(e: E) = e
}

object PermConversionAction extends Action[PermElementLike, Perm] {
  override def hashCode = 0xfefebaba
  override def equals(that: Any) = that == this
  def apply(e: PermElementLike) = e.explicit
}

case class ActionGroup[
  G <: FiniteGroup[F],
  F <: FiniteElement[F],
  P <: PermElement[P]](g: G, a: Action[F, P]) extends PermGroup[ActionElement[F, P]] {
  type Element = ActionElement[F, P]
  def degree = a(g.identity).size
  def compatible(e: Element) = g.compatible(e.f)
  def contains(e: Element) = g.contains(e.f)
  def elements = g.elements.map(ActionElement(_, a))
  def generators = g.generators.map(ActionElement(_, a))
  def identity = ActionElement(g.identity, a)
  def order = g.order
  def randomElement(gen: Random) = ActionElement(g.randomElement(gen), a)
  def fromExplicit(p: Perm) = elements.find(_.explicit === p)
}

case class ActionElement[F <: FiniteElement[F], P <: PermElement[P]](f: F, a: Action[F, P]) extends PermElement[ActionElement[F, P]] {
  type Element = ActionElement[F, P]
  def source = f
  def compatible(that: Element) = a == that.a
  def *(that: Element) = {
    require_(compatible(that))
    ActionElement(f*(that.f), a)
  }
  def ===(that: Element) = {
    require_(compatible(that))
    f === that.f
  }
  override def hashCode() = f.hashCode()
  def inverse = ActionElement(f.inverse, a)
  def isIdentity = f.isIdentity
  def explicit = Perm(images)
  def image(k: Dom) = a(f).image(k)
  def invImage(k: Dom) = a(f).invImage(k)
  def images = a(f).images
  def size = a(f).size
}
