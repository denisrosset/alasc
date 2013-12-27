/*
# Classes for permutation domain elements #

This solves the problem of dealing with mixed 0-based or 1-based indices by
defining an abstract element, that can be then retrieved as a 0-based or 1-based
integer.

The class `Dom` is implemented as a value class extending `AnyVal` (feature of Scala 2.10)
and thus has no overhead.
*/

package net.alasc

import scala.collection.IndexedSeqLike
import scala.collection.mutable.{Builder, ArrayBuffer}
import scala.collection.generic.CanBuildFrom

/*
## `Dom` class for domain elements.

Value class for Domain elements, allowing the user to retrieve 0- or 1-based integer
indices.

Note: never use ==, but instead === to compare a `Dom` value to an `Int`,
because === will apply implicit conversions, and == will not.
*/

class Dom private[alasc] (val zeroBased: Int) extends AnyVal {
  override def toString = (zeroBased + Dom.startIndex).toString
  def _1: Int = zeroBased + 1
  def _0: Int = zeroBased
  def **[P <: PermElement[P]](p: P) = p.image(this)
  def ===(that: Dom) = zeroBased == that.zeroBased
  def next = new Dom(zeroBased + 1)
}

object Dom {
  object IntOrder {
    implicit object DomOrdering extends Ordering[Dom] {
      def compare(a: Dom, b: Dom) = a.zeroBased - b.zeroBased
    }
    implicit class OrderedDom(d: Dom) extends Ordered[Dom] {
      def compare(that: Dom) = d.zeroBased - that.zeroBased
    }
  }
  def _0(zeroBased: Int) = new Dom(zeroBased)
  def _1(oneBased: Int) = new Dom(oneBased - 1)
  def apply(oneBased: Int) = new Dom(oneBased - startIndex)
  var startIndex = 1
  object ZeroBased {
    import scala.language.implicitConversions
    implicit def intToDom(k: Int) = Dom._0(k)
  }
  object OneBased {
    import scala.language.implicitConversions
    implicit def intToDom(k: Int) = Dom._1(k)
  }
}

/*
## Array of `Dom` elements

This class avoids boxing the `Dom` elements in a collection.
*/

final class DomArray private[alasc] (val array: Array[Int]) extends IndexedSeq[Dom] with IndexedSeqLike[Dom, DomArray] {
  import DomArray._

  override protected[this] def newBuilder: Builder[Dom, DomArray] = DomArray.newBuilder

  def apply(i0: Int): Dom = Dom._0(array(i0))

  override def foreach[U](f: Dom => U): Unit =
    array.map(Dom._0(_)).foreach(f)

  def length = array.length
  def _0 = zeroBased
  def _1 = oneBased
  def zeroBased = array
  def oneBased: Array[Int] = array.map(_+1)
  override def toList = array.toList.map(Dom._0(_))
}

object DomArray {
  def fromSeq(images: Seq[Dom]): DomArray = new DomArray(images.map(_._0).toArray)

  def apply(images: Dom*) = fromSeq(images)

  def newBuilder: Builder[Dom, DomArray] =
    new ArrayBuffer mapResult fromSeq

  implicit def canBuildFrom: CanBuildFrom[DomArray, Dom, DomArray] =
    new CanBuildFrom[DomArray, Dom, DomArray] {
      def apply(): Builder[Dom, DomArray] = newBuilder
      def apply(from: DomArray): Builder[Dom, DomArray] = newBuilder
    }
  def _0(arr: Array[Int]): DomArray = fromZeroBasedArray(arr)
  def _1(arr: Array[Int]): DomArray = fromOneBasedArray(arr)
  def _0(seq: Seq[Int]): DomArray = fromZeroBasedSeq(seq)
  def _1(seq: Seq[Int]): DomArray = fromOneBasedSeq(seq)

  def fromZeroBasedArray(arr: Array[Int]) = new DomArray(arr.clone)
  def fromOneBasedArray(arr: Array[Int]) = new DomArray(arr.map(_-1))

  def fromZeroBasedSeq(seq: Seq[Int]) = new DomArray(seq.toArray)
  def fromOneBasedSeq(seq: Seq[Int]) = new DomArray(seq.map(_-1).toArray)
}
