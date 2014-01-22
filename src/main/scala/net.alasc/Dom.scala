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

trait DomTrait extends Any with Ordered[DomTrait] {
  def compare(that: DomTrait): Int
  def _0: Int
  def _1: Int
  def **(g: Perm): DomTrait
  def ===(that: DomTrait)
  def hash: Int
  def next: DomTrait
}

trait DomImplicits {
  implicit def +(dom: Dom, shift: Int): Dom
  def -(otherDom: Dom): Int
}

/*
## `Dom` class for domain elements.

Value class for Domain elements, allowing the user to retrieve 0- or 1-based integer
indices.

Note: never use ==, but instead === to compare a `Dom` value to an `Int`,
because == could apply implicit conversions, and === will not.
*/

class Dom private[alasc] (val zeroBased: Int) extends AnyVal with DomTrait {
  def compare(that: DomTrait) = zeroBased.compare(that._0)
  def compare(that: Dom) = zeroBased.compare(that.zeroBased)
  override def toString = (zeroBased + Dom.startIndex).toString
  def _1: Int = zeroBased + 1
  def _0: Int = zeroBased
  def **(g: Perm): Dom = g.image(this)
  def **[P <: Permuting[P]](p: P) = p.image(this)
  def !==(that: Dom) = zeroBased != that.zeroBased
  def !==(that: DomTrait) = zeroBased != that._0
  def ===(that: Dom) = zeroBased == that.zeroBased
  def ===(that: DomTrait) = zeroBased == that._0
  def next = new Dom(zeroBased + 1)
  def +(shift: Int): Dom = new Dom(zeroBased + shift)
  def -(otherDom: Dom): Int = zeroBased - otherDom.zeroBased
  def hash = zeroBased
}

object Dom {
  def domain(size: Int): Iterable[Dom] = (0 until size).map(Dom._0(_))
  def first = Dom._1(1)
  def last(size: Int) = Dom._1(size)
  def _0(zeroBased: Int) = new Dom(zeroBased)
  def _1(oneBased: Int) = new Dom(oneBased - 1)
  def apply(startIndexBased: Int) = new Dom(startIndexBased - startIndex)
  var startIndex = 1
  object ZeroBased {
    import scala.language.implicitConversions
    implicit def intToDom(k: Int) = Dom._0(k)
    implicit def domToInt(k: Dom) = k._0
  }
  object OneBased {
    import scala.language.implicitConversions
    implicit def intToDom(k: Int) = Dom._1(k)
    implicit def domToInt(k: Dom) = k._1
  }
}
