package net.alasc.util

import scala.language.implicitConversions

/** Option class for reference types, using `null` as a special value for `None`. */
final class RefOption[+A <: AnyRef](val a: A) extends AnyVal {
  override def toString = if (a eq null) "none" else s"some($a)"
  def isEmpty = a eq null
  def isDefined = a ne null
  def get: A = if (a ne null) a else sys.error("RefOption is empty")

  def getOrElse[B >: A](default: => B): B =
    if (isEmpty) default else get

  def orNull: A = a

  def nonEmpty = isDefined

  def contains[A1 >: A](elem: A1): Boolean =
    !isEmpty && a == elem

  def exists(p: A => Boolean): Boolean =
    !isEmpty && p(a)

  def forall(p: A => Boolean): Boolean = isEmpty || p(a)

  def foreach[U](f: A => U): Unit =
    if (!isEmpty) f(a)

  def iterator: Iterator[A] =
    if (isEmpty) collection.Iterator.empty else collection.Iterator.single(a)

  def toList: List[A] =
    if (isEmpty) List() else new ::(a, Nil)

  def toRight[X](left: => X) =
    if (isEmpty) Left(left) else Right(a)

  def toLeft[X](right: => X) =
    if (isEmpty) Right(right) else Left(a)

  def toOption: Option[A] = Option(a)
}

object RefOption {
  def apply[A <: AnyRef](a: A): RefOption[A] = new RefOption(a)
  def unapply[A <: AnyRef](ro: RefOption[A]): RefOption[A] = ro
}

trait RefOptionTopLevel {
  final def RefSome[A <: AnyRef](a: A): RefOption[A] = new RefOption(a)
  final def RefNone: RefOption[Null] = new RefOption[Null](null)
}
