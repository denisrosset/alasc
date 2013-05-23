package com.faacets
package perm

/** Value class for Domain elements. Allows the user to specify 0- or 1-based domains.
  * 
  * @note Never use ==, but === to compare a Dom value to an Int, because === will apply
  *       implicit conversions, and == will not.
  */
class Dom private[perm] (val zeroBased: Int) extends AnyVal {
  override def toString = (zeroBased + Dom.startIndex).toString
  def _1: Int = zeroBased + 1
  def _0: Int = zeroBased
  def **[P <: PermElement[P]](p: P) = p.image(this)
  def ===(that: Dom) = zeroBased == that.zeroBased
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
