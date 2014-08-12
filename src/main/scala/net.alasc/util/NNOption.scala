package net.alasc.util

import scala.language.implicitConversions

/** Option class for non-negative integers, with -1 representing `none`. All other
  * negative values are illegal.
  */
class NNOption(val i: Int) extends AnyVal { lhs =>
  override def toString = if (i < 0) "none" else s"some($i)"
  @inline def isEmpty = i < 0
  @inline def isDefined = i >= 0
  @inline def get: Int = if (isDefined) i else sys.error("NNOption is empty")

  @inline final def getOrElse(default: => Int): Int =
    if (isEmpty) default else get

  @inline final def fold[B](ifEmpty: => B)(f: Int => B): B =
    if (isEmpty) ifEmpty else f(i)

  @inline final def nonEmpty = isDefined

  @inline final def contains(elem: Int): Boolean =
    !isEmpty && i == elem

  @inline final def exists(p: Int => Boolean): Boolean =
    !isEmpty && p(i)

  @inline final def forall(p: Int => Boolean): Boolean = isEmpty || p(i)

  @inline final def foreach[U](f: Int => U): Unit =
    if (!isEmpty) f(i)

  def iterator: Iterator[Int] =
    if (isEmpty) collection.Iterator.empty else collection.Iterator.single(i)

  @inline def toList: List[Int] =
    if (isEmpty) List() else new ::(i, Nil)

  @inline final def toRight[X](left: => X) =
    if (isEmpty) Left(left) else Right(i)

  @inline final def toLeft[X](right: => X) =
    if (isEmpty) Right(right) else Left(i)

  @inline final def toOption: Option[Int] =
    if (isEmpty) None else Some(i)

  @inline final def reduceMin(rhs: NNOption) =
    if (i < 0) rhs
    else if (rhs.i < 0) this
    else NNSome(lhs.i.min(rhs.i))

  @inline final def reduceMax(rhs: NNOption) =
    if (i < 0) rhs
    else if (rhs.i < 0) this
    else NNSome(lhs.i.max(rhs.i))
}

object NNOption {
  implicit def nnOptionToOption(nno: NNOption): Option[Int] = nno.toOption
  @inline def apply(i: Int): NNOption = if (i >= 0) new NNOption(i) else NNNone
}

trait NNOptionTopLevel {
  @inline final def NNSome(i: Int): NNOption = {
    require(i >= 0)
    new NNOption(i)
  }
  @inline final def NNNone: NNOption = new NNOption(-1)
}
