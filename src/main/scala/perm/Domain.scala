package com.faacets
package perm

class Domain private[perm] (val zeroBased: Int) extends AnyVal with Ordered[Domain] {
  override def toString = value.toString
  def value: Int = zeroBased + 1
  def compare(that: Domain) = zeroBased - that.zeroBased
  def **[P <: PermElement[P]](p: P) = p.image(this)
}

object Domain {
  def zeroBased(zeroBased: Int) = new Domain(zeroBased)
  def apply(oneBased: Int) = new Domain(oneBased - 1)
}
