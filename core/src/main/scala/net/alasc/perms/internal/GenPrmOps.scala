package net.alasc.perms.internal

import spire.syntax.cfor.cforRange
import net.alasc.perms.Cycles

class GenPrmOps(val lhs: GenPrm) extends AnyVal {

  def image(preimage: Int): Int =
    if (preimage >= lhs.length) preimage else lhs(preimage)

  def invImage(image: Int): Int =
    if (image >= lhs.length) image else {
      cforRange(0 until lhs.length) { i =>
        if (lhs(i) == image) return i
      }
      sys.error("Corrupted permutation")
    }

  def smallestMovedPoint: Int = {
    cforRange(0 until lhs.length) { i =>
      if (lhs(i) != i) return i
    }
    -1
  }

  def largestMovedPoint: Int = {
    var i = lhs.length - 1
    while (i >= 0) {
      if (lhs(i) != i) return i
      i -= 1
    }
    -1
  }

  def nMovedPoints = {
    var n = 0
    cforRange(0 until lhs.length) { i =>
      if (lhs(i) != i) n += 1
    }
    n
  }

  def movedPoints = {
    import metal.syntax._
    val bitset = metal.mutable.FixedBitSet.reservedSize(lhs.length)
    cforRange(0 until lhs.length) { i =>
      if (lhs(i) != i) bitset += i
    }
    bitset.toScala
  }

  def toCycles = Cycles.fromSupportAndImageFun(movedPoints, image(_))

}
