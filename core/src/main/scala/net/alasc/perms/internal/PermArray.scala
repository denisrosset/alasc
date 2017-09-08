package net.alasc.perms
package internal

import spire.syntax.cfor._

import metal.syntax._

import net.alasc.util._

/** Permutation represented by an array of images, the array length being minimal with
  * respect to the permutation support, i.e. :
  * 
  * - images(images.length - 1) != images.length - 1.
  */
final class PermArray(val images: Array[Int]) extends PermBase { lhs =>
  require(images(images.length - 1) != images.length - 1)

  def isId = {
    assert(images.length > Perm16Encoding.movedPointsUpperBound + 1)
    false
  }

  def inverse: PermArray = {
    val array = new Array[Int](images.length)
    var k = largestMovedPoint.get
    while (k >= 0) {
      array(images(k)) = k
      k -= 1
    }
    new PermArray(array)
  }

  def image(preimage: Int) =
    if (preimage > largestMovedPoint.get) preimage else images(preimage)

  def invImage(i: Int): Int =
    if (i > largestMovedPoint.get) i else {
      var k = images.length - 1
      while (k >= 0) {
        if (images(k) == i)
          return k
        k -= 1
      }
      sys.error("Invalid permutation")
    }

  @inline def largestMovedPoint = NNSome(images.length - 1)

  def smallestMovedPoint: NNOption = {
    var k = 0
    if (largestMovedPoint.isEmpty) return NNNone
    val sm = largestMovedPoint.get
    while (k <= sm && images(k) == k)
      k += 1
    assert(k != images.length)
    NNSome(k)
  }

  def movedPoints = {
    val bitset = metal.mutable.ResizableBitSet.empty
    var k = largestMovedPoint.getOrElseFast(-1)
    while (k >= 0) {
      if (image(k) != k)
        bitset += k
      k -= 1
    }
    bitset.toScala
  }

  def nMovedPoints = {
    var n = 0
    var k = largestMovedPoint.getOrElseFast(-1)
    while (k >= 0) {
      if (image(k) != k)
        n += 1
      k -= 1
    }
    n
  }

  def isValidPerm32 = largestMovedPoint.getOrElseFast(-1) <= Perm32Encoding.supportMaxElement

  def toPerm32 = {
    assert(isValidPerm32)
    val res = new Perm32
    var k = images.length - 1
    while (k >= 0) {
      Perm32Encoding.encode(res, k, image(k))
      k -= 1
    }
    res
  }

  override def genEqv(rhs: AbstractPerm): Boolean = rhs match {
    case rhs1: PermArray => (lhs.images.length == rhs1.images.length) && {
      cforRange(0 until lhs.images.length) { k =>
        if (lhs.images(k) != rhs1.images(k)) return false
      }
      true
    }
    case _ => super.genEqv(rhs)
  }
}

object PermArray extends PermCompanion {
  @inline def movedPointsUpperBound = Int.MaxValue - 1

  def fromImagesAndHighSupportMax(images: Array[Int], supportMax: Int): PermArray = {
    val minimalSizeImages = new Array[Int](supportMax + 1)
    Array.copy(images, 0, minimalSizeImages, 0, supportMax + 1)
    new PermArray(minimalSizeImages)
  }

  def fromHighSupportAndImageFun(support: Set[Int], image: Int => Int, supportMax: Int): PermArray =
    new PermArray(Array.tabulate(supportMax + 1)(k => if (support(k)) image(k) else k))
}
