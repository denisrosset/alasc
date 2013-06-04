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
  override def equals(that: Any) = that.asInstanceOf[AnyRef] eq this
  def apply(e: PermElementLike) = e.explicit
}

case class ActionElement[F <: FiniteElement[F], P <: PermElement[P]](f: F, a: Action[F, P]) extends PermElement[ActionElement[F, P]] {
  type Element = ActionElement[F, P]
  lazy val p = a(f)
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
  def image(k: Dom) = p.image(k)
  def invImage(k: Dom) = p.invImage(k)
  def images = p.images
  def size = p.size
}
