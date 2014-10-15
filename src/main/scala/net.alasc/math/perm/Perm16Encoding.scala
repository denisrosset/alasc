package net.alasc.math
package perm

import scala.collection.immutable

import spire.syntax.groupAction._
import spire.syntax.signed._

import net.alasc.algebra.Permutation
import net.alasc.syntax.permutationAction._
import net.alasc.util._

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
  @inline def supportMin(encoding: Long): NNOption =
    if (encoding == 0) NNNone else NNSome(numberOfTrailingZeros(encoding)/4)
  @inline def supportMax(encoding: Long): NNOption =
    if (encoding == 0) NNNone else NNSome(15 - numberOfLeadingZeros(encoding)/4)

  @inline def supportMaxElement = 15
  @inline def idEncoding: Long = 0L
  @inline def id: Perm16 = new Perm16(idEncoding)
  @inline def invImage(encoding: Long, i: Int): Int = {
    val low = supportMin(encoding).getOrElseFast(-1)
    if (i < low) return i
    var k = supportMax(encoding).getOrElseFast(0)
    if (i > k) return i
    while (k >= low) {
      if (decode(encoding, k) == i)
        return k
      k -= 1
    }
    sys.error("Invalid permutation")
  }

  @inline def image(encoding: Long, preimage: Int): Int =
    if (preimage > supportMaxElement) preimage else decode(encoding, preimage)

  def support(encoding: Long): Set[Int] = {
    var bitset = 0L
    var remaining = encoding
    while (remaining != 0) {
      val preimage = Perm16Encoding.supportMin(remaining).get
      val image = Perm16Encoding.decode(remaining, preimage)
      bitset |= 1 << preimage
      bitset |= 1 << image
      remaining &= ~(Perm16Encoding.permMask(image) | Perm16Encoding.permMask(preimage))
    }
    immutable.BitSet.fromBitMask(Array(bitset))
  }

  def inverse(encoding: Long): Long = {
    if (encoding >= 0 && encoding <= 0xFF) return encoding
    val low = supportMin(encoding).get
    var k = supportMax(encoding).get
    var res = 0L
    while (k >= low) {
      res |= encode(decode(encoding, k), k)
      k -= 1
    }
    res
  }

  @inline def minus(encoding: Long, n: Int): Long = {
    assert(n >= 0 && n <= 16)
    assert((encoding & LongBits.rightFill(n*4)) == 0)
    encoding >>> (n*4)
  }

  @inline def plus(encoding: Long, n: Int): Long = {
    assert(n >= 0 && n <= 16)
    assert(n <= 16)
    assert((encoding & LongBits.leftFill(n*4)) == 0)
    encoding << (n*4)
  }

  @inline def op(lhs: Long, rhs: Long): Long = {
    // both are either identity or (0,1)
    if (lhs >= 0L && lhs <= 0xFFL && rhs >= 0L && rhs <= 0xFFL) return lhs ^ rhs

    val low = supportMin(lhs | rhs).get
    var k = supportMax(lhs | rhs).get
    var res = 0L
    while (k >= low) {
      res |= encode(k, decode(rhs, decode(lhs, k)))
      k -= 1
    }
    res
  }

  def imagesEncoding(images: Seq[Int], supportMax: Int = 15): Long = {
    var encoding = 0L
    var k = supportMax
    assert(k <= supportMaxElement)
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
      assert(k <= supportMaxElement && i <= supportMaxElement)
      encoding |= encode(k, i)
    }
    encoding
  }
}
