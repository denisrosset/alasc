package net.alasc.math
package perm

import scala.collection.mutable

import spire.syntax.signed._

import net.alasc.algebra._
import net.alasc.util._

/** Permutation represented by an array of images, the array length being minimal with
  * respect to the permutation support, i.e. :
  * 
  * - images(images.length - 1) != images.length - 1.
  */
final class PermArray(val images: Array[Int]) extends PermBase {
  require(images(images.length - 1) != images.length - 1)
  def isId = {
    assert(images.length > Perm16Encoding.supportMaxElement + 1)
    false
  }

  def inverse: PermArray = {
    val array = new Array[Int](images.length)
    var k = supportMax.get
    while (k >= 0) {
      array(images(k)) = k
      k -= 1
    }
    new PermArray(array)
  }

  def image(preimage: Int) =
    if (preimage > supportMax.get) preimage else images(preimage)

  def invImage(i: Int): Int =
    if (i > supportMax.get) i else {
      var k = images.length - 1
      while (k >= 0) {
        if (images(k) == i)
          return k
        k -= 1
      }
      sys.error("Invalid permutation")
    }

  @inline def supportMax = NNSome(images.length - 1)

  def supportMin: NNOption = {
    var k = 0
    if (supportMax.isEmpty) return NNNone
    val sm = supportMax.get
    while (k <= sm && images(k) == k)
      k += 1
    assert(k != images.length)
    NNSome(k)
  }

  def support = {
    val bitset = mutable.BitSet.empty
    var k = supportMax.getOrElse(-1)
    while (k >= 0) {
      if (image(k) != k)
        bitset += k
      k -= 1
    }
    bitset.toImmutable
  }

  def isValidPerm32 = supportMax.getOrElse(-1) <= Perm32Encoding.supportMaxElement

  def toPerm32 = {
    assert(isValidPerm32)
    var res = new Perm32
    var k = images.length - 1
    while (k >= 0) {
      Perm32Encoding.encode(res, k, image(k))
      k -= 1
    }
    res
  }

  override def genEqv(rhs: AbstractPerm): Boolean = rhs match {
    case rhs1: PermArray => images.sameElements(rhs1.images)
    case _ => super.genEqv(rhs)
  }
}

object PermArray extends PermCompanion {
  @inline def supportMaxElement = Int.MaxValue - 1

  def fromImagesAndHighSupportMax(images: Seq[Int], supportMax: Int): PermArray =
    new PermArray(images.view.take(supportMax + 1).toArray)

  def fromHighSupportAndImageFun(support: Set[Int], image: Int => Int, supportMax: Int): PermArray =
    new PermArray(Array.tabulate(supportMax + 1)(k => if (support(k)) image(k) else k))
}
