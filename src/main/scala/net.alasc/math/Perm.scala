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

abstract class Perm {
  def toCycles: Cycles = Cycles.Algebra.fromSupportAndImages(support, image(_))
  override def toString = toCycles.toString

  protected def fastImage(preimage: Int): Int

  def image(preimage: Int): Int
  def invImage(image: Int): Int

  def inverse: Perm

  def supportMax: Int
  def supportMin: Int
  def support: BitSet

  def isValidPerm16: Boolean
  def isValidPerm32: Boolean
  def toPerm16: Perm16
  def toPerm32: Perm32
  def toPermArray = PermArray.Algebra.fromSupportAndImages(support, image(_))
}

object Perm {
  implicit def permutation: Permutation[Perm] = Algebra
  implicit val Algebra: BuildablePermutation[Perm] = new PermPermutation
}

final class PermPermutation extends BuildablePermutation[Perm] {
  def id = Perm16.Algebra.id

  def supportMaxElement = PermArray.Algebra.supportMaxElement

  def fromImages(images: Seq[Int]): Perm = {
    var maxSupport = images.length - 1
    while (maxSupport >= 0 && images(maxSupport) == maxSupport)
      maxSupport -= 1
    if (maxSupport == -1)
      id
    else if (maxSupport < 16)
      Perm16.Algebra.fromImages(images.take(16))
    else if (maxSupport < 32)
      Perm32.Algebra.fromImages(images.take(32))
    else
      PermArray.Algebra.fromImages(images)
  }

  @tailrec def fromSupportAndImages(support: BitSet, image: Int => Int): Perm =
    if (support.isEmpty)
      id
    else {
      val supportMax = support.max
      if (image(supportMax) == supportMax)
        fromSupportAndImages(support - supportMax, image)
      else if (supportMax < 16)
        Perm16.Algebra.fromSupportAndImages(support, image)
      else if (supportMax < 32)
        Perm32.Algebra.fromSupportAndImages(support, image)
      else
        PermArray.Algebra.fromSupportAndImages(support, image)
    }

  def eqv(x: Perm, y: Perm): Boolean = (x, y) match {
    case (x1: PermArray, y1: PermArray) => x1 === y1
    case (x1: Perm16, y1: Perm16) => x1 === y1
    case (x1: Perm32, y1: Perm32) => x1 === y1

    case (x1: Perm32, y1: Perm16) if x1.isValidPerm16 => x1.toPerm16 === y1
    case (x1: Perm32, y1: Perm16) => false
    case (x1: PermArray, y1: Perm16) if x1.isValidPerm16 => x1.toPerm16 === y1
    case (x1: PermArray, y1: Perm16) => false

    case (x1: Perm16, y1: Perm32) if y1.isValidPerm16 => x1 === y1.toPerm16
    case (x1: Perm16, y1: Perm32) => false
    case (x1: PermArray, y1: Perm32) if x1.isValidPerm32 => x1.toPerm32 === y1
    case (x1: PermArray, y1: Perm32) => false

    case (x1: Perm16, y1: PermArray) if y1.isValidPerm16 => x1 === y1.toPerm16
    case (x1: Perm16, y1: PermArray) => false
    case (x1: Perm32, y1: PermArray) if y1.isValidPerm32 => x1 === y1.toPerm32
    case (x1: Perm32, y1: PermArray) => false
  }

  def inverse(a: Perm) = a.inverse

  def signum(a: Perm): Int = a match {
    case a1: Perm16 => a1.signum
    case a1: Perm32 => a1.signum
    case a1: PermArray => a1.signum
  }

  override def actl(p: Perm, k: Int) = p.invImage(k)
  def actr(k: Int, p: Perm) = p.image(k)

  def support(a: Perm) = a.support
  def supportMax(p: Perm) = p.supportMax
  def supportMin(p: Perm) = p.supportMin


  def op(lhs: Perm, rhs: Perm): Perm = {
    var maxSup = lhs.supportMax.max(rhs.supportMax)
    def img(k: Int): Int = rhs.image(lhs.image(k))
    while (maxSup >= 0 && img(maxSup) == maxSup)
      maxSup -= 1
    if (maxSup == -1)
      id
    else if (maxSup <= Perm16.Algebra.supportMaxElement) {
      var res = 0L
      while (maxSup >= 0) {
        res += Perm16Encoding.encode(maxSup, img(maxSup))
        maxSup -= 1
      }
      new Perm16(new Perm16Val(res))
    } else if (maxSup <= Perm32.Algebra.supportMaxElement) {
      val res = new Perm32
      while (maxSup >= 0) {
        res.encode(maxSup, img(maxSup))
        maxSup -= 1
      }
      res
    } else {
      val array = new Array[Int](maxSup + 1)
      while (maxSup >= 0) {
        array(maxSup) = img(maxSup)
        maxSup -= 1
      }
      new PermArray(array)
    }
  }

  def plus(lhs: Perm, n: Int): Perm =
    if (n < 0)
      minus(lhs, -n)
    else if (n == 0)
      lhs
    else {
      var maxSup = lhs.supportMax + n
      if (maxSup <= Perm16.Algebra.supportMaxElement) {
        var res = 0L
        while (maxSup >= n) {
          res += Perm16Encoding.encode(maxSup, lhs.image(maxSup - n) + n)
          maxSup -= 1
        }
        new Perm16(new Perm16Val(res))
      } else if (maxSup <= Perm32.Algebra.supportMaxElement) {
        val res = new Perm32
        while (maxSup >= n) {
          res.encode(maxSup, lhs.image(maxSup - n) + n)
          maxSup -= 1
        }
        res
      } else {
        val array = new Array[Int](maxSup + 1)
        while (maxSup >= n) {
          array(maxSup) = lhs.image(maxSup - n) + n
          maxSup -= 1
        }
        while (maxSup >= 0) {
          array(maxSup) = maxSup
          maxSup -= 1
        }
        new PermArray(array)
      }
    }

  def minus(lhs: Perm, n: Int): Perm =
    if (n < 0)
      plus(lhs, -n)
    else if (n == 0)
      lhs
    else {
      if (n > lhs.supportMin)
        sys.error(s"Cannot shift down by $n the permutation, because ${lhs.supportMin} is in the support.")
      var maxSup = lhs.supportMax - n
      if (maxSup <= Perm16.Algebra.supportMaxElement) {
        var res = 0L
        while (maxSup >= 0) {
          res += Perm16Encoding.encode(maxSup, lhs.image(maxSup + n) - n)
          maxSup -= 1
        }
        new Perm16(new Perm16Val(res))
      } else if (maxSup <= Perm32.Algebra.supportMaxElement) {
        val res = new Perm32
        while (maxSup >= 0) {
          res.encode(maxSup, lhs.image(maxSup + n) - n)
          maxSup -= 1
        }
        res
      } else {
        val array = new Array[Int](maxSup + 1)
        while (maxSup >= 0) {
          array(maxSup) = lhs.image(maxSup + n) - n
          maxSup -= 1
        }
        new PermArray(array)
      }
    }
}
