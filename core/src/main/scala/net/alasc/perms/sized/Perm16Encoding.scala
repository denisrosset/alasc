package net.alasc.perms.sized

import scala.collection.immutable

import spire.syntax.cfor.cforRange
import net.alasc.perms.Perm
import net.alasc.perms.internal.Prm

/** Permutation with domain in [0, 15] encoded in a Long used as a bit string.
  * 
  * The encoding is as follows:
  * 
  * - for k = 0..15, the image of k is encoded using four bits starting at position 4*k
  * - the value of these 4 bits do not give the image k' itself, rather the shift (k' - k)
  */
object Perm16Encoding {

  import java.lang.Long.{numberOfLeadingZeros, numberOfTrailingZeros}

  @inline def permMask(preimage: Int): Long = (0xF.toLong << (preimage * 4))

  @inline def encode(preimage: Int, image: Int): Long =
    ((image - preimage) & 0xF).toLong << (preimage * 4)

  @inline def decode(encoding: Long, preimage: Int): Int =
    ((preimage + (encoding >>> (preimage*4))) & 0x0F).toInt

  def smallestMovedPoint(encoding: Long): Int =
    if (encoding == 0) -1 else numberOfTrailingZeros(encoding)/4

  def largestMovedPoint(encoding: Long): Int =
    if (encoding == 0) -1 else 15 - numberOfLeadingZeros(encoding)/4

  @inline def movedPointsUpperBound = 15

  @inline def idEncoding: Long = 0L

  def invImage(encoding: Long, i: Int): Int = {
    val low = smallestMovedPoint(encoding)
    if (i < low) return i
    var k = largestMovedPoint(encoding)
    if (k == -1) k = 0
    if (i > k) return i
    while (k >= low) {
      if (decode(encoding, k) == i)
        return k
      k -= 1
    }
    sys.error("Invalid permutation")
  }

  @inline def image(encoding: Long, preimage: Int): Int =
    if (preimage > movedPointsUpperBound) preimage else decode(encoding, preimage)

  def movedPoints(encoding: Long): Set[Int] = {
    var bitset = 0L
    var remaining = encoding
    while (remaining != 0) {
      val preimage = Perm16Encoding.smallestMovedPoint(remaining)
      val image = Perm16Encoding.decode(remaining, preimage)
      bitset |= 1 << preimage
      bitset |= 1 << image
      remaining &= ~(Perm16Encoding.permMask(image) | Perm16Encoding.permMask(preimage))
    }
    immutable.BitSet.fromBitMask(Array(bitset))
  }

  def nMovedPoints(encoding: Long): Int = {
    val mask = 0x1111111111111111L
    val antimask = ~mask
    var rest = encoding
    var bits = encoding
    rest = (rest & antimask) >>> 1
    bits |= rest
    rest = (rest & antimask) >>> 1
    bits |= rest
    rest = (rest & antimask) >>> 1
    bits |= rest
    java.lang.Long.bitCount(bits & mask)
  }

  def inverse(encoding: Long): Long = {
    if (encoding >= 0 && encoding <= 0xFF) return encoding
    val low = smallestMovedPoint(encoding)
    var k = largestMovedPoint(encoding)
    var res = 0L
    while (k >= low) {
      res |= encode(decode(encoding, k), k)
      k -= 1
    }
    res
  }

  def op(lhs: Long, rhs: Long): Long = {
    // both are either identity or (0,1)
    if (lhs >= 0L && lhs <= 0xFFL && rhs >= 0L && rhs <= 0xFFL) return lhs ^ rhs

    val low = smallestMovedPoint(lhs | rhs)
    var k = largestMovedPoint(lhs | rhs)
    var res = 0L
    while (k >= low) {
      res |= encode(k, decode(rhs, decode(lhs, k)))
      k -= 1
    }
    res
  }

  def toPerm32(encoding: Long): Perm32 = {
    val res = new Perm32
    var k = largestMovedPoint(encoding)
    val low = smallestMovedPoint(encoding)
    while (k >= low) {
      Perm32Encoding.encode(res, k, decode(encoding, k))
      k -= 1
    }
    res
  }

  def fromPrm(prm: Prm): Long = {
    import net.alasc.perms.internal.implicits._
    var k = prm.largestMovedPoint
    if (k == -1) return idEncoding
    var encoding = 0L
    if (k >= 16) sys.error("Prm does not fit into Perm16")
    val low = prm.smallestMovedPoint
    while (k >= low) {
      val i = prm.image(k)
      encoding |= encode(k, i)
      k -= 1
    }
    encoding
  }

  def toPrm(encoding: Long): Prm = {
    val n = Perm16Encoding.largestMovedPoint(encoding) + 1
    val images = new Array[Int](n)
    cforRange(0 until n) { i => images(i) = image(encoding, i) }
    images.asInstanceOf[Prm]
  }

  def imagesEncoding(images: Array[Int], supportMax: Int = 15): Long = {
    var encoding = 0L
    var k = supportMax
    assert(k <= movedPointsUpperBound)
    while (k >= 0) {
      val i = images(k)
      encoding |= encode(k, i)
      k -= 1
    }
    encoding
  }

  def supportAndImageFunEncoding(support: Set[Int], image: Int => Int): Long = {
    var encoding = 0L
    support.foreach { k =>
      val i = image(k)
      assert(k <= movedPointsUpperBound && i <= movedPointsUpperBound)
      encoding |= encode(k, i)
    }
    encoding
  }

}
