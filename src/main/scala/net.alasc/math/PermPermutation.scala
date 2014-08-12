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
  @inline def supportMin(p: Perm): NNOption = p.supportMin
  @inline def supportMax(p: Perm): NNOption = p.supportMax
  @inline def actr(preimage: Int, p: Perm): Int = p.image(preimage)
  @inline override def actl(p: Perm, i: Int): Int = p.invImage(i)
  @inline def signum(p: Perm): Int = p.to[Cycles].signum // TODO: could be optimized
  @inline def inverse(p: Perm): Perm = p.inverse
  @inline def id = Perm16Encoding.id
  @inline def supportMaxElement = PermArray.supportMaxElement
  @inline def fromImages(images: Seq[Int]): Perm = Perm.fromImages(images)
  @inline def fromSupportAndImageFun(support: BitSet, image: Int => Int): Perm =
    Perm.fromSupportAndImageFun(support, image)
}
