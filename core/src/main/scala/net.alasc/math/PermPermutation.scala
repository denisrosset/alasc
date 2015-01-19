package net.alasc.math

import scala.annotation.tailrec
import scala.collection.immutable

import spire.syntax.eq._
import spire.syntax.signed._
import spire.syntax.group._
import spire.syntax.action._

import net.alasc.algebra._
import net.alasc.syntax.permutationAction._
import net.alasc.util._
import perm._

final class PermPermutation extends ShiftablePermutation[Perm] {
  def eqv(x: Perm, y: Perm): Boolean = x match {
    case lhs16: Perm16 => y match {
      case rhs16: Perm16 => lhs16.encoding == rhs16.encoding
      case _ => false
    }
    case lhs32: Perm32 => y match {
      case _: Perm16 => false
      case rhs32: Perm32 => 
        lhs32.long2 == rhs32.long2 && lhs32.long1 == rhs32.long1 && lhs32.long0 == rhs32.long0
      case rhs: PermBase => rhs.genEqv(lhs32)
    }
    case lhs: PermBase => y match {
      case _: Perm16 => false
      case rhs: AbstractPerm => lhs.genEqv(rhs)
    }
  }
  def op(x: Perm, y: Perm): Perm = x match {
    case lhs16: Perm16 => y match {
      case rhs16: Perm16 => new Perm16(Perm16Encoding.op(lhs16.encoding, rhs16.encoding))
      case rhs32: Perm32 => Perm32Encoding.op1632(lhs16, rhs32)
      case rhs: Perm if lhs16.encoding == 0L => rhs
      case rhs: PermBase => rhs.genRevOp(x)
    }
    case lhs32: Perm32 => y match {
      case rhs16: Perm16 => Perm32Encoding.op3216(lhs32, rhs16)
      case rhs32: Perm32 => Perm32Encoding.op3232(lhs32, rhs32)
      case rhs: PermBase => rhs.genRevOp(x)
    }
    case lhs: PermBase => y match {
      case Perm16(0L) => lhs
      case rhs: Perm => lhs.genOp(rhs)
    }
  }
  def support(p: Perm): Set[Int] = p.support
  def supportMin(p: Perm): NNOption = p.supportMin
  def supportMax(p: Perm): NNOption = p.supportMax
  def actr(preimage: Int, p: Perm): Int = p.image(preimage)
  override def actl(p: Perm, i: Int): Int = p.invImage(i)
  override def signum(p: Perm): Int = p.to[Cycles].signum
  def inverse(p: Perm): Perm = p.inverse
  val id = Perm16Encoding.id
  def supportMaxElement = PermArray.supportMaxElement
  def fromImages(images: Seq[Int]): Perm = Perm.fromImages(images)
  def fromSupportAndImageFun(support: Set[Int], image: Int => Int): Perm =
    Perm.fromSupportAndImageFun(support, image)
  def plus(p: Perm, n: Int): Perm = {
    require(n >= 0)
    if (n == 0) return p
    val newSupport = immutable.BitSet.empty ++ support(p).map(_ + n)
    fromSupportAndImageFun(newSupport, k => actr(k - n, p) + n)
  }
  def minus(p: Perm, n: Int): Perm = {
    require(n >= 0)
    if (n == 0) return p
    require(p.supportMin.getOrElse(n) >= n)
    val newSupport = immutable.BitSet.empty ++ support(p).map(_ - n)
    fromSupportAndImageFun(newSupport, k => actr(k + n, p) - n)
  }
}
