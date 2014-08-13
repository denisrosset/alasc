package net.alasc.math
package perm

import scala.collection.immutable.BitSet

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
    var bitset = BitSet.empty
    var k = supportMax.getOrElse(-1)
    while (k >= 0) {
      if (image(k) != k)
        bitset += k
      k -= 1
    }
    bitset
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

  def fromHighSupportAndImageFun(support: BitSet, image: Int => Int, supportMax: Int): PermArray =
    new PermArray(Array.tabulate(supportMax + 1)(k => if (support(k)) image(k) else k))
}

/*

  override def hashCode: Int =
    if (isValidPerm16) toPerm16.hashCode
    else if (isValidPerm32) toPerm32.hashCode
    else {
      import scala.util.hashing.MurmurHash3.{mix, mixLast, finalizeHash}
      // TODO: add test that the underlying scala.util.MurmurHash3 implementation did not change
      var a, b, n = 0
      var c = 1
      var k = 0
      while (k < images.length) {
        if (images(k) != k) {
          val hash = pairHash(k)
          a += hash
          b ^= hash
          if (hash != 0) c *= hash
          n += 1
        }
      }
      var h = PermHash.seed
      h = mix(h, a)
      h = mix(h, b)
      h = mixLast(h, c)
      finalizeHash(h, n)
    }

  def specOp(rhs: PermArray): PermArray = {
    if (lhs.images.length == 0) return rhs
    if (rhs.images.length == 0) return lhs
    var ind = lhs.supportMax.max(rhs.supportMax)
    @inline def img(k: Int) = rhs.image(lhs.image(k))
    while (ind >= 0 && img(ind) == ind)
      ind -= 1
    if (ind == -1)
      PermArray.Algebra.id
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

  def minus(n: Int): PermArray =
    if (n == 0) lhs
    else if (n < 0) plus(-n)
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

  def plus(n: Int): PermArray =
    if (n == 0) lhs
    else if (n < 0) minus(-n)
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
    }*/


