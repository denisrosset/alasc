package net.alasc.math

import net.alasc.algebra._
import scala.collection.immutable.BitSet
import spire.syntax.signed._
import spire.syntax.groupAction._
import perm.{Perm16, Perm32, PermArray}

abstract class Perm {
  def toCycles: Cycles = Cycles.fromSupportAndImages(support, image(_))
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
  def toPermArray = PermArray.fromSupportAndImages(support, image(_))
}

object Perm extends PermutationBuilder[Perm] {
  implicit val permutation = new PermPermutation
  def supportMaxElement = PermArray.supportMaxElement
  def fromImages(images: Seq[Int]): Perm = {
    var maxSupport = images.length - 1
    while (maxSupport >= 0 && images(maxSupport) == maxSupport)
      maxSupport -= 1
    if (maxSupport == -1)
      permutation.id
    else if (maxSupport < 16)
      Perm16.fromImages(images.take(16))
    else if (maxSupport < 32)
      Perm32.fromImages(images.take(32))
    else
      PermArray.fromImages(images)
  }

  def fromSupportAndImages(support: BitSet, image: Int => Int): Perm =
    if (support.isEmpty)
      permutation.id
    else {
      val supportMax = support.max
      if (image(supportMax) == supportMax)
        fromSupportAndImages(support - supportMax, image)
      else if (supportMax < 16)
        Perm16.fromSupportAndImages(support, image)
      else if (supportMax < 32)
        Perm32.fromSupportAndImages(support, image)
      else
        PermArray.fromSupportAndImages(support, image)
    }
}
