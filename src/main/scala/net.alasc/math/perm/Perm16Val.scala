package net.alasc.math
package perm

import net.alasc.algebra.{Permutation, PermutationBuilder}
import scala.collection.immutable.BitSet
import net.alasc.syntax.permutation._
import spire.syntax.groupAction._
import spire.syntax.signed._

object LongBits {
  @inline final def rightFill(n: Int): Long = ((1L << n) - 1) - ((n & 64) >> 6)
  @inline final def leftFill(n: Int): Long = ~rightFill(64 - n)
}

object Perm16Encoding {
  import java.lang.Long.{numberOfLeadingZeros, numberOfTrailingZeros}

  @inline final def permMask(preimage: Int): Long = (0xF << (preimage * 4))
  @inline final def encode(preimage: Int, image: Int): Long =
    ((image - preimage) & 0xF).toLong << (preimage * 4)
  @inline final def decode(encoding: Long, preimage: Int): Int =
    ((preimage + (encoding >>> (preimage*4))) & 0x0F).toInt
  @inline final def supportMin(encoding: Long): Int = 
    if (encoding == 0) -1 else numberOfTrailingZeros(encoding)/4
  @inline final def supportMax(encoding: Long): Int =
    if (encoding == 0) -1 else 15 - numberOfLeadingZeros(encoding)/4
}

/** Permutation with domain in [0, 15] encoded in a Long used as a bit string.
  * 
  * The encoding is as follows:
  * 
  * - for k = 0..15, the image of k is encoded using four bits starting at position 4*k
  * - the value of these 4 bits do not give the image k' itself, rather the shift (k' - k)
  */
class Perm16Val private[math](val encoding: Long) extends AnyVal { lhs =>
  def toPerm32: Perm32 = {
    var k = Perm16Encoding.supportMax(encoding)
    val res = new Perm32
    while (k >= 0) {
      res.encode(k, Perm16Val.permutation.actr(k, this))
      k -= 1
    }
    res
 }

  def toCycles = Cycles.fromPermutation(this)(Perm16Val.permutation).toString

  override def toString = toCycles.toString

  protected[math] def fastImage(preimage: Int): Int = Perm16Encoding.decode(encoding, preimage)

  def invImage(i: Int): Int = {
    val low = supportMin
    if (i < low) return i
    var k = supportMax
    if (i > k) return i
    while (k >= low) {
      if (fastImage(k) == i)
        return k
      k -= 1
    }
    sys.error("Invalid permutation")
  }

  @inline def image(preimage: Int): Int =
    if (preimage >= 16) preimage else fastImage(preimage)

  def support: BitSet = {
    var bitset = 0L
    var remaining = encoding
    while (remaining != 0) {
      val preimage = Perm16Encoding.supportMin(remaining)
      val image = Perm16Encoding.decode(remaining, preimage)
      bitset |= 1 << preimage
      bitset |= 1 << image
      remaining &= ~(Perm16Encoding.permMask(image) | Perm16Encoding.permMask(preimage))
    }
    BitSet.fromBitMask(Array(bitset))
  }

  def supportMin = Perm16Encoding.supportMin(encoding)
  def supportMax = Perm16Encoding.supportMax(encoding)

  def inverse: Perm16Val = {
    if (encoding >= 0 && encoding <= 0xFF) return this
    val low = supportMin
    var k = supportMax
    var res = 0L
    while (k >= low) {
      res += Perm16Encoding.encode(fastImage(k), k)
      k -= 1
    }
    new Perm16Val(res)
  }

  def |+|(rhs: Perm16Val): Perm16Val = {
    if (lhs.encoding == 0) return rhs
    if (rhs.encoding == 0) return lhs
    val low = Perm16Encoding.supportMin(lhs.encoding | rhs.encoding)
    var k = Perm16Encoding.supportMax(lhs.encoding | rhs.encoding)
    var res = 0L
    while (k >= low) {
      res += Perm16Encoding.encode(k, rhs.fastImage(lhs.fastImage(k)))
      k -= 1
    }
    new Perm16Val(res)
  }

  def ===(rhs: Perm16Val): Boolean = lhs.encoding == rhs.encoding

  def -(n: Int): Perm16Val =
    if (n < 0) lhs + (-n)
    else if (n == 0) lhs
    else {
      assert(n <= 16)
      assert((encoding & LongBits.rightFill(n*4)) == 0)
      new Perm16Val(encoding >>> (n*4))
    }

  def +(n: Int): Perm16Val =
    if (n < 0) lhs - (-n)
    else if (n == 0) lhs
    else {
      assert(n <= 16)
      assert((encoding & LongBits.leftFill(n*4)) == 0)
      new Perm16Val(encoding << (n*4))
    }
}

class Perm16ValPermutation extends Permutation[Perm16Val] {
  def eqv(x: Perm16Val, y: Perm16Val): Boolean = x === y
  def id = new Perm16Val(0L)
  def op(x: Perm16Val, y: Perm16Val): Perm16Val = x |+| y
  def signum(a: Perm16Val) = Cycles.fromPermutation(a)(this).signum
  def inverse(p: Perm16Val) = p.inverse
  def support(p: Perm16Val) = p.support
  def supportMax(p: Perm16Val) = p.supportMax
  def supportMin(p: Perm16Val) = p.supportMin
  def actl(g: Perm16Val, i: Int): Int = g.invImage(i)
  def actr(k: Int, g: Perm16Val): Int = g.image(k)
  def minus(p: Perm16Val, n: Int): Perm16Val = p - n
  def plus(p: Perm16Val, n: Int): Perm16Val = p + n
}

object Perm16Val extends PermutationBuilder[Perm16Val] {
  def supportMaxElement = 15

  implicit val permutation = new Perm16ValPermutation

  def fromImages(images: Seq[Int]): Perm16Val = {
    var k = 0
    var encoding = 0L
    val n = images.length
    assert(n <= 16)
    while (k < n) {
      val i = images(k)
      encoding += Perm16Encoding.encode(k, i)
      k += 1
    }
    new Perm16Val(encoding)
  }

  def fromSupportAndImages(support: BitSet, image: Int => Int): Perm16Val = {
    var encoding = 0L
    support.foreach { k =>
      val i = image(k)
      assert(k <= 15 && i <= 15)
      encoding += Perm16Encoding.encode(k, i)
    }
    new Perm16Val(encoding)
  }
}
