package net.alasc.math
package perm

import net.alasc.algebra._
import scala.collection.immutable.BitSet
import net.alasc.syntax.permutation._
import spire.syntax.groupAction._
import spire.syntax.signed._
import spire.syntax.eq._
import spire.syntax.group._

/** Class for permutation with images in domain 0...15.
  * 
  * Is a wrapper for the value class Perm16Val.
  */
final class Perm16(val permVal: Perm16Val) extends SpecPerm[Perm16] { lhs =>
  def isValidPerm16 = true
  def isValidPerm32 = true
  def toPerm16 = this
  protected[math] def fastImage(preimage: Int) = permVal.fastImage(preimage)
  def toPerm32 = permVal.toPerm32
  def supportMin = permVal.supportMin
  def supportMax = permVal.supportMax
  def support = permVal.support
  def inverse = new Perm16(permVal.inverse)
  def image(preimage: Int) = permVal.image(preimage)
  def invImage(k: Int) = permVal.invImage(k)
  def specEqv(rhs: Perm16) = lhs.permVal === rhs.permVal
  def specOp(rhs: Perm16): Perm16 = new Perm16(lhs.permVal |+| rhs.permVal)
  def specPlus(n: Int) = new Perm16(permVal + n)
  def specMinus(n: Int) = new Perm16(permVal - n)
}

final class Perm16Permutation extends PermPermutationBase[Perm16] {
  def id = new Perm16(Perm16Val.Algebra.id)
  def fromImages(images: Seq[Int]) = new Perm16(Perm16Val.Algebra.fromImages(images))
  def fromSupportAndImages(support: BitSet, image: Int => Int) =
    new Perm16(Perm16Val.Algebra.fromSupportAndImages(support, image))
  def supportMaxElement = Perm16Val.Algebra.supportMaxElement
}

object Perm16 {
  implicit val Algebra = new Perm16Permutation
}
