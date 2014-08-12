package net.alasc.util

import scala.language.implicitConversions

/** Option class for reference types, using `null` as a special value for `None`. */
class RefOption[+A <: AnyRef](val a: A) extends AnyVal {
  override def toString = if (a eq null) "none" else s"some($a)"
  @inline def isEmpty = a eq null
  @inline def isDefined = a ne null
  @inline def get: A = if (a ne null) a else sys.error("RefOption is empty")

  @inline final def getOrElse[B >: A](default: => B): B =
    if (isEmpty) default else get

  @inline final def orNull: A = a

  @inline final def fold[B](ifEmpty: => B)(f: A => B): B =
    if (isEmpty) ifEmpty else f(a)

  @inline final def nonEmpty = isDefined

  @inline final def contains[A1 >: A](elem: A1): Boolean =
    !isEmpty && a == elem

  @inline final def exists(p: A => Boolean): Boolean =
    !isEmpty && p(a)

  @inline final def forall(p: A => Boolean): Boolean = isEmpty || p(a)

  @inline final def foreach[U](f: A => U): Unit =
    if (!isEmpty) f(a)

  def iterator: Iterator[A] =
    if (isEmpty) collection.Iterator.empty else collection.Iterator.single(a)

  @inline def toList: List[A] =
    if (isEmpty) List() else new ::(a, Nil)

  @inline final def toRight[X](left: => X) =
    if (isEmpty) Left(left) else Right(a)

  @inline final def toLeft[X](right: => X) =
    if (isEmpty) Right(right) else Left(a)
}

object RefOption {
  implicit def refOptionToOption[A <: AnyRef](ro: RefOption[A]): Option[A] = Option(ro.a)
  @inline def apply[A <: AnyRef](a: A): RefOption[A] = new RefOption(a)
}

trait RefOptionTopLevel {
  @inline final def RefSome[A <: AnyRef](a: A): RefOption[A] = new RefOption(a)
  @inline final def RefNone: RefOption[Null] = new RefOption[Null](null)
}
