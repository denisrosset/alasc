package net.alasc.math
package perm

import net.alasc.algebra._
import scala.collection.immutable.BitSet
import spire.syntax.signed._

/** Permutation represented by an array of images, the array length being minimal with
  * respect to the permutation support, i.e. :
  * 
  * - images(images.length - 1) != images.length - 1.
  */
final class PermArray(val images: Array[Int]) extends SpecPerm[PermArray] { lhs =>

  protected[alasc] def fastImage(preimage: Int) = images(preimage)

  def image(preimage: Int) =
    if (preimage > supportMax) preimage else images(preimage)

  def invImage(i: Int): Int =
    if (i >= images.length) i else {
      var k = images.length - 1
      while (k >= 0) {
        if (fastImage(k) == i)
          return k
        k -= 1
      }
      sys.error("Invalid permutation")
    }

  @inline def supportMax = images.length - 1
  def supportMin = {
    var k = 0
    while (k < images.length && images(k) == k)
      k += 1
    if (k == images.length) -1 else k
  }
  def support = {
    var bitset = BitSet.empty
    var k = supportMax
    while (k >= 0) {
      if (image(k) != k)
        bitset += k
      k -= 1
    }
    bitset
  }
  def isValidPerm16 = images.length <= 16
  def isValidPerm32 = images.length <= 32
  def toPerm16 = {
    var encoding = 0L
    assert(isValidPerm16)
    var k = images.length - 1
    while (k >= 0) {
      encoding += Perm16Encoding.encode(k, images(k))
      k -= 1
    }
    new Perm16(new Perm16Val(encoding))
  }
  def toPerm32 = {
    var res = new Perm32
    assert(isValidPerm32)
    var k = images.length - 1
    while (k >= 0) {
      res.encode(k, images(k))
      k -= 1
    }
    res
 }

  def inverse: PermArray = {
    val array = new Array[Int](images.length)
    var k = images.length - 1
    while (k >= 0) {
      array(fastImage(k)) = k
      k -= 1
    }
    new PermArray(array)
  }

  def specOp(rhs: PermArray): PermArray = {
    if (lhs.images.length == 0) return rhs
    if (rhs.images.length == 0) return lhs
    var ind = lhs.supportMax.max(rhs.supportMax)
    @inline def img(k: Int) = rhs.image(lhs.image(k))
    while (ind >= 0 && img(ind) == ind)
      ind -= 1
    if (ind == -1)
      PermArray.empty
    else {
      val array = new Array[Int](ind + 1)
      while (ind >= 0) {
        array(ind) = img(ind)
        ind -= 1
      }
      new PermArray(array)
    }
  }
  def specEqv(rhs: PermArray) = lhs.images.sameElements(rhs.images)

  def specMinus(n: Int): PermArray =
    if (n == 0) lhs
    else if (n < 0) specPlus(-n)
    else {
      assert(n <= supportMin)
      val array = new Array[Int](images.length - n)
      var k = images.length - n - 1
      while (k >= 0) {
        array(k) = fastImage(k + n) - n
        k -= 1
      }
      new PermArray(array)
    }

  def specPlus(n: Int): PermArray =
    if (n == 0) lhs
    else if (n < 0) specMinus(-n)
    else {
      val array = new Array[Int](images.length + n)
      var k = images.length + n - 1
      while (k >= n) {
        array(k) = fastImage(k - n) + n
        k -= 1
      }
      while (k >= 0) {
        array(k) = k
        k -= 1
      }
      new PermArray(array)
    }

}

class PermArrayPermutation extends PermPermutationBase[PermArray] {
  def id = PermArray.empty
}

object PermArray extends PermutationBuilder[PermArray] {
  implicit val permutation = new PermArrayPermutation
  def empty = new PermArray(Array.empty[Int])
  def supportMaxElement = Int.MaxValue
  def fromImages(images: Seq[Int]): PermArray = {
    var maxSupport = images.length - 1
    while (maxSupport >= 0 && images(maxSupport) == maxSupport)
      maxSupport -= 1
    new PermArray(Array.tabulate(maxSupport + 1)(k => images(k)))
  }
  @annotation.tailrec def fromSupportAndImages(support: BitSet, image: Int => Int): PermArray =
    if (support.isEmpty) empty else {
      val maxSupport = support.max
      if (image(maxSupport) == maxSupport)
        fromSupportAndImages(support - maxSupport, image)
      else
        new PermArray(Array.tabulate(maxSupport + 1)(k => image(k)))
    }
}
