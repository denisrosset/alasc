package net.alasc.util

import scala.collection.mutable

class MutableBitSet(elems0: Array[Long]) extends mutable.BitSet {
  def this(initSize: Int) = this(new Array[Long]((initSize + 63) >> 6 max 1))
  def this() = this(0)
  def foreachFast(f: Int => Unit) = {
    var i = 0
    while (i < nwords) {
      var w = word(i)
      var j = i * 64
      while (w != 0L) {
        if ((w&1L) == 1L) f(j)
        w = w >>> 1
        j += 1
      }
      i += 1
    }
  }
}

object MutableBitSet {
  def empty: MutableBitSet = new MutableBitSet
  def apply(elems: Int*): MutableBitSet = {
    val bs = empty
    elems.foreach { k => bs += k }
    bs
  }
  def fromBitMask(elems: Array[Long]): MutableBitSet = {
    val len = elems.length
    val a = new Array[Long](len)
    Array.copy(elems, 0, a, 0, len)
    new MutableBitSet(a)
  }
  def fromBitMaskNoCopy(elems: Array[Long]): MutableBitSet = new MutableBitSet(elems)
}
