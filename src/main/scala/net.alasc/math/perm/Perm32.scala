package net.alasc.math
package perm

import net.alasc.algebra._
import scala.collection.immutable.BitSet
import spire.syntax.signed._
import spire.syntax.groupAction._

object Perm32Encoding {
  @inline def maskWidth = 5
  @inline def longMask = 0x0FFFFFFF
  @inline def numPerLong = 12
  @inline def leftBlank = 64 - numPerLong * maskWidth
  @inline def long1Start = numPerLong
  @inline def long2Start = numPerLong * 2
  @inline def mask = 0x1FL

  import java.lang.Long.{numberOfLeadingZeros, numberOfTrailingZeros}

  @inline def supportMin(long2: Long, long1: Long, long0: Long) =
    if (long0 != 0)
      numberOfTrailingZeros(long0) / maskWidth
    else if (long1 != 0)
      long1Start + numberOfTrailingZeros(long1) / maskWidth
    else if (long2 != 0)
      long2Start + numberOfTrailingZeros(long2) / maskWidth
    else
      -1

  @inline def supportMax(long2: Long, long1: Long, long0: Long) =
    if (long2 != 0)
      31 - (numberOfLeadingZeros(long2) - 24) / maskWidth
    else if (long1 != 0)
      long2Start - 1 - (numberOfLeadingZeros(long1) - leftBlank) / maskWidth
    else if (long0 != 0)
      long1Start - 1 - (numberOfLeadingZeros(long0) - leftBlank) / maskWidth
    else
      -1
}

/** 5 bits per permutation image shift * 32 images = 160 bits.
  * 
  * - long1 stores image shifts for indices  0...11 in bits 0...59
  * - long2 stores image shifts for indices 12...23 in bits 0...59
  * - long3 stores image shifts for indices 24...31 in bits 0...39
  */
final class Perm32(
  var long2: Long = 0L,
  var long1: Long = 0L,
  var long0: Long = 0L
) extends SpecPerm[Perm32] { lhs =>
  import Perm32Encoding._
  import LongBits._

  def specEqv(rhs: Perm32): Boolean =
    (lhs.long0 == rhs.long0) && (lhs.long1 == rhs.long1) && (lhs.long2 == rhs.long2)

  // long2 contains indices 12..23, and the indices 12..15 occupy the right-most 20 bits
  def isValidPerm16 = long2 == 0 && (long1 & leftFill(44)) == 0
  def isValidPerm32 = true
  def toPerm16: Perm16 = {
    var k = Perm32Encoding.supportMax(long2, long1, long0)
    if (k >= 16) sys.error("Cannot fit in Perm16.")
    var encoding = 0L
    var l1 = long1
    while (k >= 12) {
      encoding += (l1 & 0xF) << (k * 4)
      l1 = l1 >>> maskWidth
      k -= 1
    }
    var l0 = long0
    while (k >= 0) {
      encoding += (l0 & 0xF) << (k * 4)
      l0 = l0 >> maskWidth
    }
    new Perm16(new Perm16Val(encoding))
  }
  def toPerm32 = this

  def support = {
    var bitset = 0L
    if (long2 != 0) {
      var k = 31
      while (k >= 24) {
        if (inSupport(k))
          bitset |= 1L << k
        k -= 1
      }
    }
    if (long1 != 0) {
      var k = 23
      while (k >= 12) {
        if (inSupport(k))
          bitset |= 1L << k
        k -= 1
      }
    }
    if (long0 != 0) {
      var k = 11
      while (k >= 0) {
        if (inSupport(k))
          bitset |= 1L << k
        k -= 1
      }
    }
    BitSet.fromBitMask(Array(bitset))
  }

  def supportMax = Perm32Encoding.supportMax(long2, long1, long0)
  def supportMin = Perm32Encoding.supportMin(long2, long1, long0)

  protected[math] def fastImage(preimage: Int): Int =
    if (preimage < long1Start)
      ((preimage + (long0 >>> (preimage * maskWidth))) & mask).toInt
    else if (preimage < long2Start)
      ((preimage + (long1 >>> ((preimage - long1Start) * maskWidth))) & mask).toInt
    else
      ((preimage + (long2 >>> ((preimage - long2Start) * maskWidth))) & mask).toInt

  def image(preimage: Int) =
    if (preimage > Perm32.Algebra.supportMaxElement) preimage else fastImage(preimage)

  def invImage(i: Int): Int = {
    if ((long2 == 0 && long1 == 0 && long0 == 0) || i > Perm32.Algebra.supportMaxElement)
      return i
    var k = Perm32Encoding.supportMax(long2, long1, long0)
    if (i > k) return i
    val low = Perm32Encoding.supportMin(long2, long1, long0)
    if (i < low) return i
    while (k >= low) {
      if (fastImage(k) == i)
        return k
      k -= 1
    }
    sys.error("Invalid permutation")
  }

  protected[math] def encode(preimage: Int, image: Int): Unit =
    if (preimage < long1Start)
      long0 += ((image - preimage).toLong & mask) << (preimage * maskWidth)
    else if (preimage < long2Start)
      long1 += ((image - preimage).toLong & mask) << ((preimage - long1Start) * maskWidth)
    else
      long2 += ((image - preimage).toLong & mask) << ((preimage - long2Start) * maskWidth)

  protected[math] def inSupport(preimage: Int): Boolean =
    if (preimage < long1Start)
      ((long0 >>> (preimage * maskWidth)) & mask) != 0
    else if (preimage < long2Start)
      ((long1 >>> ((preimage - long1Start) * maskWidth)) & mask) != 0
    else if (preimage <= Perm32.Algebra.supportMaxElement)
      ((long2 >>> ((preimage - long2Start) * maskWidth)) & mask) != 0
    else
      false

  def inverse: Perm32 = {
    if (long0 == 0 && long1 == 0 && long2 == 0) return this
    val low = supportMin
    var k = supportMax
    val res = new Perm32
    while (k >= low) {
      res.encode(fastImage(k), k)
      k -= 1
    }
    res
  }

  def isIdentity = long0 == 0 && long1 == 0 && long2 == 0

  def specOp(rhs: Perm32): Perm32 = {
    if (lhs.isIdentity) return rhs
    if (rhs.isIdentity) return lhs
    val low = Perm32Encoding.supportMin(lhs.long2 | rhs.long2, lhs.long1 | rhs.long1, lhs.long0 | rhs.long0)
    var k = Perm32Encoding.supportMax(lhs.long2 | rhs.long2, lhs.long1 | rhs.long1, lhs.long0 | rhs.long0)
    val res = new Perm32
    while (k >= low) {
      res.encode(k, rhs.fastImage(lhs.fastImage(k)))
      k -= 1
    }
    res
  }

  def specMinus(n: Int): Perm32 =
    if (n >= Perm32.Algebra.supportMaxElement)
      sys.error(s"Does not support shifts of more than ${Perm32.Algebra.supportMaxElement} positions.")
    else if (n == 0)
      lhs
    else if (n < 0)
      specPlus(-n)
    else if (n >= long1Start) {
      assert(long0 == 0)
      new Perm32(0L, long2, long1).specMinus(n - long1Start)
    } else {
      assert((long0 & LongBits.rightFill(n * maskWidth)) == 0)
      val nBits = n * maskWidth
      val leftShift = (numPerLong - n) * maskWidth
      val rFill = rightFill(nBits)
      new Perm32(long2 >>> nBits,
        ((long2 & rFill) << leftShift) + (long1 >>> nBits),
        ((long1 & rFill) << leftShift) + (long0 >>> nBits))
    }

  def specPlus(n: Int): Perm32 =
    if (n >= Perm32.Algebra.supportMaxElement)
      sys.error(s"Does not support shifts of more than ${Perm32.Algebra.supportMaxElement} positions.")
    else if (n == 0)
      lhs
    else if (n < 0)
      specMinus(-n)
    else if (n >= long1Start) {
      assert(long2 == 0)
      new Perm32(long1, long0, 0L).specPlus(n - long1Start)
    } else {
      val nBits = n * maskWidth
      val rightShift = (numPerLong - n) * maskWidth
      new Perm32(((long2 << nBits) & longMask) + (long1 >>> rightShift),
        ((long1 << nBits) & longMask) + (long0 >>> rightShift),
        (long0 << nBits) & longMask)
    }
}

final class Perm32Permutation extends PermPermutationBase[Perm32] {
  def id = new Perm32

  def supportMaxElement = 31

  def fromImages(images: Seq[Int]): Perm32 = {
    var k = 0
    val res = new Perm32
    val n = images.length
    assert(n <= 32)
    while (k < n) {
      val i = images(k)
      res.encode(k, i)
      k += 1
    }
    res
  }

  def fromSupportAndImages(support: BitSet, image: Int => Int): Perm32 = {
    val res = new Perm32
    support.foreach { k =>
      val i = image(k)
      assert(k <= 31 && i <= 31)
      res.encode(k, i)
    }
    res
  }
}

object Perm32 {
  implicit val Algebra = new Perm32Permutation
}
