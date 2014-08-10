package net.alasc.math

import net.alasc.algebra._
import scala.collection.immutable.BitSet
import spire.syntax.signed._
import spire.syntax.groupAction._
import perm._
import scala.annotation.tailrec
import net.alasc.syntax.permutation._
import spire.syntax.eq._
import spire.syntax.group._

/** Universal trait at the base of explicit permutation types.
  *
  * As the type for permutations on the domain 0..15 is a value class, a few conventions have to be
  * respected by all classes extending Perm:
  * 
  * - if, as the result of a computation/construction, the resulting permutation acts on a
  *   subset of 0...15, the returned instance type must be `Perm16`,
  * - an optimized type `Perm32` is available for permutations acting on a subset of 0...31 and 
  *   strict superset of 0..15; its usage is not mandatory,
  * - for larger permutations, `PermArray` is backed by an `Array[Int]`.
  * 
  * Users are free to define their own permutation types to free themselves of those constraints;
  * Cycles is such a type.
  */
sealed trait Perm extends Any {
  override def toString = this.to[Cycles].toString

  @inline protected final def pairHash(preimage: Int) = PermHash.pairHash(preimage, image(preimage))

  def image(preimage: Int): Int
  def invImage(image: Int): Int

  def isId: Boolean
  def inverse: Perm

  def supportMax: Int
  def supportMin: Int
  def support: BitSet

  def apply(seq: Int*): Perm = this |+| Cycles(seq: _*).to[Perm]
  def apply(cycle: String): Perm = apply(cycle.map(DomainAlphabet.map(_)): _*)

  def isValidPerm32: Boolean
  def toPerm32: Perm32

//  def plus(n: Int): Perm
//  def minus(n: Int): Perm
}

final case class Perm16 private[math](val encoding: Long) extends AnyVal with Perm { lhs16 =>
  def toPerm32 = Perm16Encoding.toPerm32(encoding)
  @inline def invImage(i: Int) = Perm16Encoding.invImage(encoding, i)
  @inline def image(i: Int) = Perm16Encoding.image(encoding, i)
  @inline def isId = encoding == 0L
  @inline def support = Perm16Encoding.support(encoding)
  @inline def supportMin = Perm16Encoding.supportMin(encoding)
  @inline def supportMax = Perm16Encoding.supportMax(encoding)
  @inline def inverse = new Perm16(Perm16Encoding.inverse(encoding))
  def isValidPerm32 = true
}

protected sealed abstract class AbstractPerm extends Perm { lhs =>
  /** Hash code for permutations: Perm16, Perm32, PermArray subclasses should provide the same
    * hashcode for equivalent permutations.
    * 
    * The hashing strategy is described in `perm.PermHash`.
    */
  override def hashCode: Int =
    PermHash.hash(this: Perm)

  override def equals(any: Any): Boolean = any match {
    case rhs: Perm => Perm.Algebra.eqv(lhs, rhs)
    case _ => false
  }
}

abstract class PermBase extends AbstractPerm {
  def inverse: PermBase

  def genOpLarge(rhs: Perm, givenSupportMax: Int): Perm =
    new PermArray(Array.tabulate(givenSupportMax + 1)( k => rhs.image(image(k)) ))

  def genRevOpLarge(lhs: Perm, givenSupportMax: Int): Perm =
    new PermArray(Array.tabulate(givenSupportMax + 1)( k => image(lhs.image(k)) ))

  def genOp(rhs: Perm): Perm = {
    var k = supportMax.max(rhs.supportMax)
    val low = supportMin.min(rhs.supportMin)
    @inline def img(preimage: Int) = rhs.image(image(preimage))
    while (k >= low) {
      val i = img(k)
      if (k != i) {
        if (k <= Perm16Encoding.supportMaxElement) {
          var encoding = Perm16Encoding.encode(k, i)
          k -= 1
          while (k >= low) {
            encoding |= Perm16Encoding.encode(k, img(k))
            k -= 1
          }
          return new Perm16(encoding)
        } else if (k <= Perm32Encoding.supportMaxElement) {
          val res = new Perm32
          Perm32Encoding.encode(res, k, i)
          k -= 1
          while (k >= low) {
            Perm32Encoding.encode(res, k, img(k))
            k -= 1
          }
          return res
        } else
          return genOpLarge(rhs, k)
      }
      k -= 1
    }
    Perm16Encoding.id
  }

  def genRevOp(lhs: Perm): Perm = {
    var k = supportMax.max(lhs.supportMax)
    val low = supportMin.min(lhs.supportMin)
    @inline def img(preimage: Int) = image(lhs.image(preimage))
    while (k >= low) {
      val i = img(k)
      if (i != k) {
        if (k <= Perm16Encoding.supportMaxElement) {
          var encoding = Perm16Encoding.encode(k, i)
          k -= 1
          while (k >= low) {
            encoding |= Perm16Encoding.encode(k, img(k))
            k -= 1
          }
          return new Perm16(encoding)
        } else if (k <= Perm32Encoding.supportMaxElement) {
          val res = new Perm32
          Perm32Encoding.encode(res, k, i)
          k -= 1
          while (k >= low) {
            Perm32Encoding.encode(res, k, img(k))
            k -= 1
          }
          return res
        } else
          return genRevOpLarge(lhs, k)
      }
      k -= 1
    }
    Perm16Encoding.id
  }

  def genEqv(rhs: Perm): Boolean = {
    val rhsSM = rhs.supportMax
    if (supportMax != rhsSM) false else {
      var k = supportMax
      while (k >= 0) {
        if (image(k) != rhs.image(k))
          return false
        k -= 1
      }
      true
    }
  }
}

final class Perm32(var long2: Long = 0L, var long1: Long = 0L, var long0: Long = 0L) extends AbstractPerm { lhs =>
  override def hashCode: Int = Perm32Encoding.hash(long2, long1, long0)

  def checkNotPerm16 =
    assert(!Perm32Encoding.isValidPerm16(long2, long1, long0))

  def isId = long2 == 0L && long1 == 0L && long0 == 0L
  def image(preimage: Int) = Perm32Encoding.image(long2, long1, long0, preimage)
  def invImage(i: Int) = Perm32Encoding.invImage(long2, long1, long0, i)
  def inverse: Perm32 = Perm32Encoding.inverse(this)
  def supportMax = Perm32Encoding.supportMax(long2, long1, long0)
  def supportMin = Perm32Encoding.supportMin(long2, long1, long0)
  def support = Perm32Encoding.support(long2, long1, long0)
  def isValidPerm32 = true
  def toPerm32 = this
}

final class PermPermutation extends BuildablePermutation[Perm] {
  @inline def eqv(x: Perm, y: Perm): Boolean = (x, y) match {
    case (lhs16: Perm16, rhs16: Perm16) => lhs16.encoding == rhs16.encoding
    case (_: Perm16, _) | (_, _: Perm16) => false // by Perm contract
    case (lhs32: Perm32, rhs32: Perm32) =>
      lhs32.long2 == rhs32.long2 && lhs32.long1 == rhs32.long1 && lhs32.long0 == rhs32.long0
    case (lhs: PermBase, rhs: Perm32) => lhs.genEqv(rhs)
    case (lhs: Perm32, rhs: PermBase) => rhs.genEqv(lhs)
    case (lhs: PermBase, rhs: PermBase) => lhs.genEqv(rhs)
  }
  @inline def op(x: Perm, y: Perm): Perm = (x, y) match {
    case (lhs16: Perm16, rhs16: Perm16) => new Perm16(Perm16Encoding.op(lhs16.encoding, rhs16.encoding))
    case (lhs: Perm, Perm16(0L)) => lhs
    case (Perm16(0L), rhs: Perm) => rhs
    case (lhs32: Perm32, rhs32: Perm32) => Perm32Encoding.op3232(lhs32, rhs32)
    case (lhs32: Perm32, rhs16: Perm16) => Perm32Encoding.op3216(lhs32, rhs16)
    case (lhs16: Perm16, rhs32: Perm32) => Perm32Encoding.op1632(lhs16, rhs32)
    case (lhs: Perm, rhs: PermBase) => rhs.genRevOp(lhs)
    case (lhs: PermBase, rhs: Perm) => lhs.genOp(rhs)
  }

  @inline def support(p: Perm): BitSet = p.support
  @inline def supportMin(p: Perm): Int = p.supportMin
  @inline def supportMax(p: Perm): Int = p.supportMax
  @inline def actr(preimage: Int, p: Perm): Int = p.image(preimage)
  @inline override def actl(p: Perm, i: Int): Int = p.invImage(i)
  @inline def signum(p: Perm): Int = p.to[Cycles].signum // TODO: could be optimized
  @inline def inverse(p: Perm): Perm = p.inverse
  @inline def id = Perm16Encoding.id
  @inline def supportMaxElement = PermArray.supportMaxElement
  @inline def fromImages(images: Seq[Int]): Perm = {
    var k = images.length - 1
    while (k >= 0 && images(k) == k)
      k -= 1
    if (k == -1)
      id
    else if (k <= Perm16Encoding.supportMaxElement)
      new Perm16(Perm16Encoding.imagesEncoding(images, k))
    else if (k <= Perm32Encoding.supportMaxElement)
      Perm32Encoding.fromImages(images, k)
    else
      PermArray.fromImages(images, k)
  }
  @inline def fromSupportAndImages(support: BitSet, image: Int => Int): Perm =
    if (support.isEmpty)
      id
    else {
      val sm = support.max
      assert(image(sm) != sm)
      if (sm <= Perm16Encoding.supportMaxElement)
        new Perm16(Perm16Encoding.supportAndImagesEncoding(support, image))
      else if (sm <= Perm32Encoding.supportMaxElement)
        Perm32Encoding.fromSupportAndImages(support, image)
      else
        PermArray.fromSupportAndImages(support, image)
    }
}

object Perm {
  def apply(cycle: String): Perm = apply(cycle.map(DomainAlphabet.map(_)): _*)
  def apply(seq: Int*): Perm = Cycles(seq: _*).to[Perm]
  implicit val Algebra: BuildablePermutation[Perm] = new PermPermutation
}
