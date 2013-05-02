package com.faacets
package perm

class Dom private[perm] (val zeroBased: Int) extends AnyVal with Ordered[Dom] {
  override def toString = (zeroBased + Dom.startIndex).toString
  def _1: Int = zeroBased + 1
  def _0: Int = zeroBased
//  def value: Int = zeroBased + Dom.startindex
  def compare(that: Dom) = zeroBased - that.zeroBased
  def **[P <: PermElement[P]](p: P) = p.image(this)
}

object Dom {
  def _0(zeroBased: Int) = new Dom(zeroBased)
  def _1(oneBased: Int) = new Dom(oneBased - 1)
  def apply(oneBased: Int) = new Dom(oneBased - startIndex)
  val startIndex = 1
}
