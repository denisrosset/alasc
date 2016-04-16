package net.alasc.perms
package internal

import scala.collection.immutable

import net.alasc.util._

/** 5 bits per permutation image shift * 32 images = 160 bits.
  * 
  * - long1 stores image shifts for indices  0...11 in bits 0...59
  * - long2 stores image shifts for indices 12...23 in bits 0...59
  * - long3 stores image shifts for indices 24...31 in bits 0...39
  */
object Perm32Encoding {
  import LongBits._

  @inline def maskWidth = 5
  //                             FEDCBA9876543210
  @inline def longMask: Long = 0x0FFFFFFFFFFFFFFFL
  @inline def mask6: Int = 0x3FFFFFFF
  @inline def numPerLong = 12
  @inline def leftBlank = 64 - numPerLong * maskWidth
  @inline def long1Start = numPerLong
  @inline def long2Start = numPerLong * 2
  @inline def mask: Long = 0x1FL
  @inline def supportMaxElement = 31

  import java.lang.Long.{numberOfLeadingZeros, numberOfTrailingZeros}

  def smallestMovedPoint(long2: Long, long1: Long, long0: Long) =
    if (long0 != 0)
      numberOfTrailingZeros(long0) / maskWidth
    else if (long1 != 0)
      long1Start + numberOfTrailingZeros(long1) / maskWidth
    else if (long2 != 0)
      long2Start + numberOfTrailingZeros(long2) / maskWidth
    else
      -1

  def largestMovedPoint(long2: Long, long1: Long, long0: Long) =
    if (long2 != 0)
      31 - (numberOfLeadingZeros(long2) - 24) / maskWidth
    else if (long1 != 0)
      long2Start - 1 - (numberOfLeadingZeros(long1) - leftBlank) / maskWidth
    else if (long0 != 0)
      long1Start - 1 - (numberOfLeadingZeros(long0) - leftBlank) / maskWidth
    else
      -1

  def hash(long2: Long, long1: Long, long0: Long) = {
    import scala.util.hashing.MurmurHash3.{mix, finalizeHash}
    var h = PermHash.seed
    h = mix(h, long0.toInt & mask6)
    h = mix(h, (long0 >> 30).toInt)
    h = mix(h, long1.toInt & mask6)
    h = mix(h, (long1 >> 30).toInt)
    h = mix(h, long2.toInt & mask6)
    h = mix(h, (long2 >> 30).toInt)
    finalizeHash(h, 6)
  }

  def movesPoint(long2: Long, long1: Long, long0: Long, preimage: Int): Boolean =
    if (preimage < long1Start)
      ((long0 >>> (preimage * maskWidth)) & mask) != 0
    else if (preimage < long2Start)
      ((long1 >>> ((preimage - long1Start) * maskWidth)) & mask) != 0
    else if (preimage <= supportMaxElement)
      ((long2 >>> ((preimage - long2Start) * maskWidth)) & mask) != 0
    else
      false

  def nMovedPoints(long2: Long, long1: Long, long0: Long): Int = {
    import java.lang.Long.bitCount
    val s2 = long2 | (long2 >>> 1) | (long2 >>> 2) | (long2 >>> 3) | (long2 >>> 4)
    val s1 = long1 | (long1 >>> 1) | (long1 >>> 2) | (long1 >>> 3) | (long1 >>> 4)
    val s0 = long0 | (long0 >>> 1) | (long0 >>> 2) | (long0 >>> 3) | (long0 >>> 4)
    val mask = 0x42108421
    bitCount(s0 & mask) + bitCount(s1 & mask) + bitCount(s2 & mask)
  }

  def movedPoints(long2: Long, long1: Long, long0: Long): Set[Int] = {
    var bitset = 0L
    if (long2 != 0) {
      var k = 31
      while (k >= 24) {
        if (movesPoint(long2, long1, long0, k))
          bitset |= 1L << k
        k -= 1
      }
    }
    if (long1 != 0) {
      var k = 23
      while (k >= 12) {
        if (movesPoint(long2, long1, long0, k))
          bitset |= 1L << k
        k -= 1
      }
    }
    if (long0 != 0) {
      var k = 11
      while (k >= 0) {
        if (movesPoint(long2, long1, long0, k))
          bitset |= 1L << k
        k -= 1
      }
    }
    immutable.BitSet.fromBitMask(Array(bitset))
  }

  def decode(long2: Long, long1: Long, long0: Long, preimage: Int): Int =
    if (preimage < long1Start)
      ((preimage + (long0 >>> (preimage * maskWidth))) & mask).toInt
    else if (preimage < long2Start)
      ((preimage + (long1 >>> ((preimage - long1Start) * maskWidth))) & mask).toInt
    else
      ((preimage + (long2 >>> ((preimage - long2Start) * maskWidth))) & mask).toInt

  def encode(perm: Perm32, preimage: Int, image: Int): Unit = {
    if (preimage < long1Start)
      perm.long0 |= ((image - preimage).toLong & mask) << (preimage * maskWidth)
    else if (preimage < long2Start)
      perm.long1 |= ((image - preimage).toLong & mask) << ((preimage - long1Start) * maskWidth)
    else
      perm.long2 |= ((image - preimage).toLong & mask) << ((preimage - long2Start) * maskWidth)
  }

  def toPerm16(long2: Long, long1: Long, long0: Long): Perm16 = {
    var k = largestMovedPoint(long2, long1, long0)
    if (k > Perm16Encoding.movedPointsUpperBound) sys.error("Cannot fit in Perm16.")
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
    new Perm16(encoding)
  }

  def image(long2: Long, long1: Long, long0: Long, preimage: Int): Int =
    if (preimage > supportMaxElement) preimage else decode(long2, long1, long0, preimage)

  def invImage(long2: Long, long1: Long, long0: Long, i: Int): Int = {
    if ((long2 == 0 && long1 == 0 && long0 == 0) || i > supportMaxElement)
      return i
    var k = largestMovedPoint(long2, long1, long0)
    if (i > k) return i
    val low = smallestMovedPoint(long2, long1, long0)
    if (i < low) return i
    while (k >= low) {
      if (decode(long2, long1, long0, k) == i)
        return k
      k -= 1
    }
    sys.error("Invalid permutation")
  }

  def inverse(p: Perm32): Perm32 = {
    if (p.long2 == 0 && p.long1 == 0 && p.long0 == 0) return p
    val low = smallestMovedPoint(p.long2, p.long1, p.long0)
    var k = largestMovedPoint(p.long2, p.long1, p.long0)
    val res = new Perm32
    while (k >= low) {
      encode(res, decode(p.long2, p.long1, p.long0, k), k)
      k -= 1
    }
    res
  }

  // long2 contains indices 12..23, and the indices 12..15 occupy the right-most 20 bits of long1
  def isValidPerm16(long2: Long, long1: Long, long0: Long) = long2 == 0 && (long1 & leftFill(44)) == 0

  def fromImages(images: Seq[Int], supportMax: Int = 31): Perm32 = {
    assert(supportMax <= supportMaxElement)
    assert(supportMax > Perm16Encoding.movedPointsUpperBound)
    var k = supportMax
    val res = new Perm32
    while (k >= 0) {
      val i = images(k)
      encode(res, k, i)
      k -= 1
    }
    res
  }

  def fromSupportAndImageFun(support: Set[Int], image: Int => Int): Perm32 = {
    val res = new Perm32
    support.foreach { k =>
      val i = image(k)
      encode(res, k, i)
    }
    res
  }

  def op3232(lhs: Perm32, rhs: Perm32): Perm = {
    val low = Perm32Encoding.smallestMovedPoint(lhs.long2 | rhs.long2, lhs.long1 | rhs.long1, lhs.long0 | rhs.long0)
    var k = Perm32Encoding.largestMovedPoint(lhs.long2 | rhs.long2, lhs.long1 | rhs.long1, lhs.long0 | rhs.long0)
    var i = 0
    assert(k > Perm16Encoding.movedPointsUpperBound)
    @inline def img(preimage: Int) = decode(rhs.long2, rhs.long1, rhs.long0, decode(lhs.long2, lhs.long1, lhs.long0, preimage))
    while (k >= low) {
      i = img(k)
      if (k != i) {
        if (k <= Perm16Encoding.movedPointsUpperBound) {
          var encoding = Perm16Encoding.encode(k, i)
          k -= 1
          while (k >= low) {
            encoding |= Perm16Encoding.encode(k, img(k))
            k -= 1
          }
          return new Perm16(encoding)
        } else {
          val res = new Perm32
          encode(res, k, i)
          k -= 1
          while (k >= low) {
            encode(res, k, img(k))
            k -= 1
          }
          return res
        }
      }
      k -= 1
    }
    Perm16Encoding.id
  }

  def reduceMin(i: Int, j: Int): Int =
    if (i < 0) j
    else if (j < 0) i
    else i.min(j)

  def reduceMax(i: Int, j: Int): Int = i.max(j)

  def op1632(lhs: Perm16, rhs: Perm32): Perm = {
    if (lhs.isId) return rhs
    val low = reduceMin(Perm16Encoding.smallestMovedPoint(lhs.encoding), Perm32Encoding.smallestMovedPoint(rhs.long2, rhs.long1, rhs.long0))
    var k = reduceMax(Perm16Encoding.largestMovedPoint(lhs.encoding), Perm32Encoding.largestMovedPoint(rhs.long2, rhs.long1, rhs.long0))
    var i = 0
    assert(k > Perm16Encoding.movedPointsUpperBound)
    @inline def img(preimage: Int) = {
      val inter = if (preimage > Perm16Encoding.movedPointsUpperBound) preimage else Perm16Encoding.decode(lhs.encoding, preimage)
      decode(rhs.long2, rhs.long1, rhs.long0, inter)
    }
    while (k >= low) {
      i = img(k)
      if (k != i) {
        if (k <= Perm16Encoding.movedPointsUpperBound) {
          var encoding = Perm16Encoding.encode(k, i)
          k -= 1
          while (k >= low) {
            encoding |= Perm16Encoding.encode(k, img(k))
            k -= 1
          }
          return new Perm16(encoding)
        } else {
          val res = new Perm32
          encode(res, k, i)
          k -= 1
          while (k >= low) {
            Perm32Encoding.encode(res, k, img(k))
            k -= 1
          }
          return res
        }
      }
      k -= 1
    }
    Perm16Encoding.id
  }

  def op3216(lhs: Perm32, rhs: Perm16): Perm = {
    if (rhs.isId) lhs
    val low = reduceMin(Perm32Encoding.smallestMovedPoint(lhs.long2, lhs.long1, lhs.long0), Perm16Encoding.smallestMovedPoint(rhs.encoding))
    var k = reduceMax(Perm32Encoding.largestMovedPoint(lhs.long2, lhs.long1, lhs.long0), Perm16Encoding.largestMovedPoint(rhs.encoding))
    var i = 0
    assert(k > Perm16Encoding.movedPointsUpperBound)
    @inline def img(preimage: Int) = {
      val inter = decode(lhs.long2, lhs.long1, lhs.long0, preimage)
      if (inter > Perm16Encoding.movedPointsUpperBound) inter else Perm16Encoding.decode(rhs.encoding, inter)
    }
    while (k >= low) {
      i = img(k)
      if (k != i) {
        if (k <= Perm16Encoding.movedPointsUpperBound) {
          var encoding = Perm16Encoding.encode(k, i)
          k -= 1
          while (k >= low) {
            encoding |= Perm16Encoding.encode(k, img(k))
            k -= 1
          }
          return new Perm16(encoding)
        } else {
          val res = new Perm32
          Perm32Encoding.encode(res, k, i)
          k -= 1
          while (k >= low) {
            Perm32Encoding.encode(res, k, img(k))
            k -= 1
          }
          return res
        }
      }
      k -= 1
    }
    Perm16Encoding.id
  }

}
