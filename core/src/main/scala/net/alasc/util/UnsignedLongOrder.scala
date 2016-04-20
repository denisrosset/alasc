package net.alasc.util

import spire.algebra.Order

object UnsignedLongOrder extends Order[Long] {
  // See http://www.drmaciver.com/2008/08/unsigned-comparison-in-javascala/
  override def eqv(i: Long, j: Long) = i == j
  override def lt(i: Long, j: Long) = (i < j) ^ (i < 0) ^ (j < 0)
  override def lteqv(i: Long, j: Long) = !gt(i, j)
  override def gt(i: Long, j: Long) = (j < i) ^ (i < 0) ^ (j < 0)
  override def gteqv(i: Long, j: Long) = !lt(i, j)
  def compare(i: Long, j: Long) =
    if (i == j) 0
    else if (lt(i, j)) -1
    else 1
}
