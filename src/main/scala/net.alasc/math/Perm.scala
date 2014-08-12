package net.alasc.math

import scala.annotation.tailrec
import scala.collection.immutable.BitSet

import spire.syntax.eq._
import spire.syntax.signed._
import spire.syntax.group._
import spire.syntax.groupAction._

import net.alasc.algebra._
import net.alasc.syntax.permutation._
import net.alasc.util._
import perm._

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

  def supportMax: NNOption
  def supportMin: NNOption
  def support: BitSet

  def apply(seq: Int*): Perm = this |+| Cycles(seq: _*).to[Perm]
  def apply(cycle: String): Perm = apply(cycle.map(DomainAlphabet.map(_)): _*)

  def isValidPerm32: Boolean
  def toPerm32: Perm32

//  def plus(n: Int): Perm
//  def minus(n: Int): Perm
}

object Perm extends PermCompanion {
  def supportMaxElement = PermArray.supportMaxElement

  implicit val Algebra: BuildablePermutation[Perm] = new PermPermutation

  def fromImagesAndHighSupportMax(images: Seq[Int], supportMax: Int): Perm =
    if (supportMax <= Perm32Encoding.supportMaxElement)
      Perm32.fromImagesAndHighSupportMax(images, supportMax)
    else
      PermArray.fromImagesAndHighSupportMax(images, supportMax)

  def fromHighSupportAndImageFun(support: BitSet, imageFun: Int => Int, supportMax: Int): Perm =
    if (supportMax <= Perm32Encoding.supportMaxElement)
      Perm32.fromHighSupportAndImageFun(support, imageFun, supportMax)
    else
      PermArray.fromHighSupportAndImageFun(support, imageFun, supportMax)

}

trait PermCompanion {
    /** Maximal support element for this permutation type. */
  def supportMaxElement: Int

  /** Constructs a permutation from a sequence of images, along with 
    * the computed maximal support element.of the sequence, i.e. for
    * k = supportMax + 1 ... images.length - 1, images(k) == k.
    *
    * @param images     Sequence of images representing the permutation
    * @param supportMax Maximal support element, must be > `Perm16.supportMaxElement`.
    */
  def fromImagesAndHighSupportMax(images: Seq[Int], supportMax: Int): Perm

  /** Constructs a permutation from a sequence of images. */
  def fromImages(images: Seq[Int]): Perm = {
    var k = images.length - 1
    while (k >= 0 && images(k) == k)
      k -= 1
    if (k == -1)
      Perm.Algebra.id
    else if (k <= Perm16Encoding.supportMaxElement)
      new Perm16(Perm16Encoding.imagesEncoding(images, k))
    else
      fromImagesAndHighSupportMax(images, k)
  }

  /** Constructs a permutation from its support and an image function, with
    * supportMax = support.max (given as not to compute it twice). 
    * 
    * @param support    Support of the permutation
    * @param image      Image function
    * @param supportMax `= support.max`, must be > `Perm16Encoding.supportMaxElement`
    * 
    * @note The following must hold for all `k` in `support`: `image(k) != k`.
    */
  def fromHighSupportAndImageFun(support: BitSet, imageFun: Int => Int, supportMax: Int): Perm

  /** Constructs a permutation from its support and an image function.
    *
    * @param support    Support of the permutation
    * @param image      Image function
    * 
    * @note The following must hold for all `k` in `support`: `image(k) != k`.
    */
  def fromSupportAndImageFun(support: BitSet, imageFun: Int => Int): Perm =
    if (support.isEmpty)
      Perm.Algebra.id
    else {
      val supportMax = support.max
      if (supportMax <= Perm16Encoding.supportMaxElement)
        new Perm16(Perm16Encoding.supportAndImageFunEncoding(support, imageFun))
      else
        fromHighSupportAndImageFun(support, imageFun, supportMax)
    }

  /** Constructs a permutation from a cycle represented as a string using the domain
    * 0..9, A..Z. */
  def apply(cycle: String): Perm = apply(cycle.map(DomainAlphabet.map(_)): _*)

  /** Constructs a permutatino from a cycle given as a variable number of arguments. */
  def apply(seq: Int*): Perm = {
    val map: Map[Int, Int] = (seq zip (seq.tail :+ seq.head)).toMap
    val support = BitSet.empty ++ seq
    fromSupportAndImageFun(support, map(_))
  }
}

final case class Perm16 private[math](val encoding: Long) extends AnyVal with Perm { lhs16 =>
  @inline def invImage(i: Int) = Perm16Encoding.invImage(encoding, i)
  @inline def image(i: Int) = Perm16Encoding.image(encoding, i)
  @inline def isId = encoding == 0L
  @inline def support = Perm16Encoding.support(encoding)
  @inline def supportMin = Perm16Encoding.supportMin(encoding)
  @inline def supportMax = Perm16Encoding.supportMax(encoding)
  @inline def inverse = new Perm16(Perm16Encoding.inverse(encoding))
  def isValidPerm32 = false
  def toPerm32 = sys.error("A Perm16 is never a valid Perm32, because all permutation with support <= 15 are Perm16.")
}

object Perm16 extends PermCompanion {
  def supportMaxElement = 15
  def tooBig(supportMax: Int) = sys.error(s"Permutation too big (supportMax = $supportMax) to be encoded in Perm16.")
  def fromHighSupportAndImageFun(support: BitSet, image: Int => Int, supportMax: Int): Perm =
    tooBig(supportMax)
  def fromImagesAndHighSupportMax(images: Seq[Int], supportMax: Int): Perm =
    tooBig(supportMax)
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

  def genOpLargeDefault(lhs: Perm, rhs: Perm, givenSupportMax: Int): Perm =
    new PermArray(Array.tabulate(givenSupportMax + 1)( k => rhs.image(lhs.image(k)) ))

  def genOpLarge(rhs: Perm, givenSupportMax: Int): Perm =
    genOpLargeDefault(this, rhs, givenSupportMax)

  def genRevOpLarge(lhs: Perm, givenSupportMax: Int): Perm =
    genOpLargeDefault(lhs, this, givenSupportMax)

  def genOpDefaultImpl(lhs: Perm, rhs: Perm, isRev: Boolean): Perm = {
    var k = lhs.supportMax.reduceMax(rhs.supportMax).getOrElse(-1)
    val low = lhs.supportMin.reduceMin(rhs.supportMin).getOrElse(0)
    @inline def img(preimage: Int) = rhs.image(lhs.image(preimage))
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
        } else {
          if (isRev)
            return genRevOpLarge(lhs, k)
          else
            return genOpLarge(rhs, k)
        }
      }
      k -= 1
    }
    Perm16Encoding.id
  }

  def genOp(rhs: Perm): Perm = genOpDefaultImpl(this, rhs, false)
  def genRevOp(lhs: Perm): Perm = genOpDefaultImpl(lhs, this, true)

  def genEqv(rhs: Perm): Boolean = {
    val lhs = this
    val lhsSM = lhs.supportMax
    val rhsSM = rhs.supportMax
    if (lhsSM != rhsSM) false else {
      var k = lhsSM.getOrElse(-1)
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

object Perm32 extends PermCompanion {
  def supportMaxElement = Perm32Encoding.supportMaxElement

  def fromImagesAndHighSupportMax(images: Seq[Int], supportMax: Int): Perm =
    Perm32Encoding.fromImages(images, supportMax)

  def fromHighSupportAndImageFun(support: BitSet, imageFun: Int => Int, supportMax: Int): Perm =
    Perm32Encoding.fromSupportAndImageFun(support, imageFun)
}
