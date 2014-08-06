package net.alasc.math

import net.alasc.algebra._
import net.alasc.syntax.permutation._
import scala.collection.immutable.BitSet
import spire.syntax.signed._
import spire.syntax.eq._
import spire.syntax.group._
import spire.syntax.groupAction._
import perm._

class PermPermutation extends Permutation[Perm] {
  def id = Perm16.permutation.id

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

  def actl(p: Perm, k: Int) = p.invImage(k)
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
    else if (maxSup <= Perm16.supportMaxElement) {
      var res = 0L
      while (maxSup >= 0) {
        res += Perm16Encoding.encode(maxSup, img(maxSup))
        maxSup -= 1
      }
      new Perm16(new Perm16Val(res))
    } else if (maxSup <= Perm32.supportMaxElement) {
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
      if (maxSup <= Perm16.supportMaxElement) {
        var res = 0L
        while (maxSup >= n) {
          res += Perm16Encoding.encode(maxSup, lhs.image(maxSup - n) + n)
          maxSup -= 1
        }
        new Perm16(new Perm16Val(res))
      } else if (maxSup <= Perm32.supportMaxElement) {
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
      if (maxSup <= Perm16.supportMaxElement) {
        var res = 0L
        while (maxSup >= 0) {
          res += Perm16Encoding.encode(maxSup, lhs.image(maxSup + n) - n)
          maxSup -= 1
        }
        new Perm16(new Perm16Val(res))
      } else if (maxSup <= Perm32.supportMaxElement) {
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
