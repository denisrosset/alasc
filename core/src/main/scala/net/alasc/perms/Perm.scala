package net.alasc.perms

import scala.collection.immutable

import spire.algebra.{Eq, Group, Order}
import spire.syntax.group._

import net.alasc.algebra._
import net.alasc.syntax.permutationAction._
import net.alasc.util._
import internal._
import net.alasc.finite.FaithfulPermutationActionBuilder

/** Universal trait at the base of explicit permutation types.
  *
  * As the type for permutations on the domain 0..15 is a value class, a few conventions have to be
  * respected by all classes extending Perm:
  * 
  * - if, as the result of a computation/construction, the resulting permutation acts on a
  *   subset of 0...15, the returned instance type must be `Perm16`,
  * - an optimized type `Perm32` is available for permutations acting on a subset of 0...31 and 
  *   strict superset of 0..15; its usage is not mandatory,
  * - for larger permutations, `PermArray` is backed by an `Array[Int]`.
  * 
  * Users are free to define their own permutation types to free themselves of those constraints;
  * Cycles is such a type.
  */
sealed trait Perm extends Any {

  override def toString =
    if (isId) "Perm.id" else "Perm" + toCycles.string

  protected final def pairHash(preimage: Int) = PermHash.pairHash(preimage, image(preimage))

  def image(preimage: Int): Int
  def invImage(image: Int): Int

  def isId: Boolean
  def inverse: Perm

  def largestMovedPoint: NNOption
  def smallestMovedPoint: NNOption
  def movedPoints: Set[Int]
  def nMovedPoints: Int

  def apply(seq: Int*): Perm = this |+| Cycles(seq: _*).toPerm
  def apply(cycle: String): Perm = apply(cycle.map(Cycle.alphabetMap(_)): _*)

  def isValidPerm32: Boolean
  def toPerm32: Perm32
  def toCycles: Cycles = Cycles.fromPerm(this)

}

object Perm extends PermCompanion {

  val id: Perm = Perm16Encoding.id

  def movedPointsUpperBound = PermArray.movedPointsUpperBound

  implicit def algebra: PermAlgebra.type = PermAlgebra

  implicit object faithfulPermutationActionBuilder extends FaithfulPermutationActionBuilder[Perm] {
    override def toString = "Perm.faithfulPAB"
    def apply(generators: Iterable[Perm]): PermutationAction[Perm] = algebra
  }

  def fromImagesAndHighSupportMax(images: Array[Int], supportMax: Int): Perm =
    if (supportMax <= Perm32Encoding.supportMaxElement)
      Perm32.fromImagesAndHighSupportMax(images, supportMax)
    else
      PermArray.fromImagesAndHighSupportMax(images, supportMax)

  def fromHighSupportAndImageFun(support: Set[Int], imageFun: Int => Int, supportMax: Int): Perm =
    if (supportMax <= Perm32Encoding.supportMaxElement)
      Perm32.fromHighSupportAndImageFun(support, imageFun, supportMax)
    else
      PermArray.fromHighSupportAndImageFun(support, imageFun, supportMax)

  def fromMap(map: Map[Int, Int]): Perm =
    fromSupportAndImageFun(map.keySet, k => map.getOrElse(k, k))

  def fromImageFun(n: Int, image: Int => Int): Perm =
    fromSupportAndImageFun(Set(0 until n:_*), image)

  def fromImages(images: Seq[Int]): Perm = images match {
    case wa: scala.collection.mutable.WrappedArray[Int] => fromImages(wa.array)
    case _ => fromImages(images.toArray)
  }

  def transposition(i: Int, j: Int): Perm =
    fromMap(Map(i -> j, j -> i))

  def sorting[T:Order](seq: Seq[T]): Perm = {
    import spire.compat._
    fromImages(seq.zipWithIndex.sortBy(_._1).map(_._2))
  }

  def random(size: Int)(implicit gen: scala.util.Random): Perm = {
    import spire.std.int._
    sorting(Seq.tabulate(size)(k => gen.nextInt))
  }


}

trait PermCompanion {

    /** Maximal support element for this permutation type. */
  def movedPointsUpperBound: Int

  /** Constructs a permutation from an array of images, along with
    * the computed maximal support element.of the sequence, i.e. for
    * k = supportMax + 1 ... images.length - 1, images(k) == k.
    *
    * The array is considered mutable and is copied if needed.
    *
    * @param images     Array of images representing the permutation
    * @param supportMax Maximal support element, must be > `Perm16.supportMaxElement`.
    */
  def fromImagesAndHighSupportMax(images: Array[Int], supportMax: Int): Perm

  /** Constructs a permutation from a sequence of images, along with
    * the computed maximal support element.of the sequence, i.e. for
    * k = supportMax + 1 ... images.length - 1, images(k) == k.
    *
    * @param images     Sequence of images representing the permutation
    * @param supportMax Maximal support element, must be > `Perm16.supportMaxElement`.
    */
  def fromImagesAndHighSupportMax(images: Seq[Int], supportMax: Int): Perm = images match {
    case wa: collection.mutable.WrappedArray[Int] => fromImagesAndHighSupportMax(wa.array, supportMax)
    case _ => fromImagesAndHighSupportMax(images.toArray, supportMax)
  }

  /** Returns a permutation from the given image array
    * (the array is considered mutable and is copied if needed). */
  def fromImages(images: Array[Int]): Perm = {
    var k = images.length - 1
    while (k >= 0 && images(k) == k)
      k -= 1
    if (k == -1)
      Perm.id
    else if (k <= Perm16Encoding.movedPointsUpperBound)
      new Perm16(Perm16Encoding.imagesEncoding(images, k))
    else
      fromImagesAndHighSupportMax(images, k)
  }

  /** Constructs a permutation from its support and an image function, with
    * supportMax = support.max (given as not to compute it twice).
    *
    * @param support    Support of the permutation
    * @param imageFun   Image function
    * @param supportMax `= support.max`, must be > `Perm16Encoding.supportMaxElement`
    * @note The following must hold for all `k` in `support`: `image(k) != k`.
    */
  def fromHighSupportAndImageFun(support: Set[Int], imageFun: Int => Int, supportMax: Int): Perm

  /** Constructs a permutation from its support and an image function.
    *
    * @param support    Support of the permutation
    * @param imageFun   Image function
    * @note The following must hold for all `k` in `support`: `image(k) != k`.
    */
  def fromSupportAndImageFun(support: Set[Int], imageFun: Int => Int): Perm =
    if (support.isEmpty)
      Perm.id
    else {
      val supportMax = support.max
      if (supportMax <= Perm16Encoding.movedPointsUpperBound)
        new Perm16(Perm16Encoding.supportAndImageFunEncoding(support, imageFun))
      else
        fromHighSupportAndImageFun(support, imageFun, supportMax)
    }

  /** Constructs a permutation from a cycle represented as a string using the domain
    * 0..9, A..Z. */
  def apply(cycle: String): Perm = apply(cycle.map(Cycle.alphabetMap(_)): _*)

  /** Constructs a permutation from a cycle given as a variable number of arguments. */
  def apply(seq: Int*): Perm = if (seq.isEmpty) Perm.id else {
    val map: Map[Int, Int] = (seq zip (seq.tail :+ seq.head)).toMap
    val support = immutable.BitSet.empty ++ seq
    fromSupportAndImageFun(support, map(_))
  }
}

/** Class for tiny permutations on domain 0..15. Can be used either standalone as a value class,
  * or as part of the `Perm` hierarchy, as it extends the universal trait `Perm`. In that case,
  * the value class will be wrapped.
  */
final case class Perm16(val encoding: Long) extends AnyVal with Perm { lhs16 =>
  def invImage(i: Int) = Perm16Encoding.invImage(encoding, i)
  def image(i: Int) = Perm16Encoding.image(encoding, i)
  def isId = encoding == 0L
  def movedPoints = Perm16Encoding.movedPoints(encoding)
  def nMovedPoints = Perm16Encoding.nMovedPoints(encoding)
  def smallestMovedPoint = NNOption(Perm16Encoding.smallestMovedPoint(encoding))
  def largestMovedPoint = NNOption(Perm16Encoding.largestMovedPoint(encoding))
  def inverse = new Perm16(Perm16Encoding.inverse(encoding))
  def isValidPerm32 = false
  def toPerm32 = sys.error("A Perm16 is never a valid Perm32, because all permutation with support <= 15 are Perm16.")
}

object Perm16 extends PermCompanion {
  def fromEncoding(encoding: Long): Perm16 = new Perm16(encoding)
  def movedPointsUpperBound = 15
  def tooBig(supportMax: Int) = sys.error(s"Permutation too big (supportMax = $supportMax) to be encoded in Perm16.")
  def fromHighSupportAndImageFun(support: Set[Int], image: Int => Int, supportMax: Int): Perm =
    tooBig(supportMax)
  def fromImagesAndHighSupportMax(images: Array[Int], supportMax: Int): Perm =
    tooBig(supportMax)
}

/** Base class for non-value class permutations. To have a base class to extend outside alasc,
  * use `PermBase` instead.
  */
protected sealed abstract class AbstractPerm extends Perm { lhs =>

  /** Hash code for permutations: Perm16, Perm32, PermArray subclasses should provide the same
    * hashcode for equivalent permutations.
    *
    * The hashing strategy is described in `perm.PermHash`.
    */
  override def hashCode: Int =
    PermHash.hash(this: Perm)

  override def equals(any: Any): Boolean = any match {
    case rhs: Perm => Perm.algebra.eqv(lhs, rhs)
    case _ => false
  }

}

/** Base class for user-defined permutation types for large permutation. A default type
  * is provided in alasc as `PermArray`.
  *
  * The user can override the `genOpLarge`, `genRevOpLarge` methods in priority. The methods
  * `genOp`, `genRevOp` can be override if the user-defined permutations have special structure
  * that enable fast computation of the support of the result.
  *
  * Have a look at the decision logic in `PermPermutation.op`.
  */
abstract class PermBase extends AbstractPerm {

  def inverse: PermBase

  def genOpLargeDefault(lhs: Perm, rhs: Perm, givenSupportMax: Int): Perm =
    new PermArray(Array.tabulate(givenSupportMax + 1)( k => rhs.image(lhs.image(k)) ))

  /** Computes `this |+| rhsGenerators`, while the maximal element of the result support
    * has already been computed by `genOp` and is greater than `Perm32.supportMaxElement`.
    */
  def genOpLarge(rhs: Perm, givenSupportMax: Int): Perm =
    genOpLargeDefault(this, rhs, givenSupportMax)

  /** Computes `lhs |+| this`, while the maximal element of the result support
    * has already been computed by `genRevOp` and is greater than `Perm32.supportMaxElement`.
    */
  def genRevOpLarge(lhs: Perm, givenSupportMax: Int): Perm =
    genOpLargeDefault(lhs, this, givenSupportMax)

  /** Default `lhs |+| rhsGenerators` product implementation, which constructs a `Perm16` or `Perm32` if possible,
    * and delegates to `genOpLarge` or `genRevOpLarge` otherwise.
    */
  def genOpDefaultImpl(lhs: Perm, rhs: Perm, isRev: Boolean): Perm = {
    var k = lhs.largestMovedPoint.reduceMax(rhs.largestMovedPoint).getOrElseFast(-1)
    val low = lhs.smallestMovedPoint.reduceMin(rhs.smallestMovedPoint).getOrElseFast(0)
    @inline def img(preimage: Int) = rhs.image(lhs.image(preimage))
    while (k >= low) {
      val i = img(k)
      if (k != i) {
        if (k <= Perm16Encoding.movedPointsUpperBound) {
          var encoding = Perm16Encoding.encode(k, i)
          k -= 1
          while (k >= low) {
            encoding |= Perm16Encoding.encode(k, img(k))
            k -= 1
          }
          return new Perm16(encoding)
        } else if (k <= Perm32Encoding.supportMaxElement) {
          val res = new Perm32
          Perm32Encoding.encode(res, k, i)
          k -= 1
          while (k >= low) {
            Perm32Encoding.encode(res, k, img(k))
            k -= 1
          }
          return res
        } else {
          if (isRev)
            return genRevOpLarge(lhs, k)
          else
            return genOpLarge(rhs, k)
        }
      }
      k -= 1
    }
    Perm16Encoding.id
  }

  /** Computes the product `this |+| rhsGenerators`, where `this` is an user-defined permutation type, and
    * `rhsGenerators` can be either a small permutation type such as `Perm16`, `Perm32`, or an user-defined type.
    */
  def genOp(rhs: Perm): Perm = genOpDefaultImpl(this, rhs, false)

  /** Computes the product `lhs |+| this`, where `this` is an user-defined permutation type, and
    * `lhs` can be either a small permutation type such as `Perm16`, `Perm32`, or an user-defined type.
    */
  def genRevOp(lhs: Perm): Perm = genOpDefaultImpl(lhs, this, true)

  /** Tests for equivalency. Can be overriden for speed; note that `rhsGenerators` can be any large permutation type,
    * such as `PermArray` or `Perm32`, but not `Perm16`, as this possibility is already ruled out
    * in `PermPermutation.eqv` logic. */
  def genEqv(rhs: AbstractPerm): Boolean = {
    val lhs = this
    val lhsSM = lhs.largestMovedPoint
    val rhsSM = rhs.largestMovedPoint
    if (lhsSM != rhsSM) false else {
      var k = lhsSM.getOrElseFast(-1)
      while (k >= 0) {
        if (image(k) != rhs.image(k))
          return false
        k -= 1
      }
      true
    }
  }

}

final class Perm32(var long2: Long = 0L, var long1: Long = 0L, var long0: Long = 0L) extends AbstractPerm { lhs =>

  override def hashCode: Int = Perm32Encoding.hash(long2, long1, long0)

  def checkNotPerm16() =
    assert(!Perm32Encoding.isValidPerm16(long2, long1, long0))

  def isId = long2 == 0L && long1 == 0L && long0 == 0L

  def image(preimage: Int) = Perm32Encoding.image(long2, long1, long0, preimage)
  def invImage(i: Int) = Perm32Encoding.invImage(long2, long1, long0, i)

  def inverse: Perm32 = Perm32Encoding.inverse(this)

  def nMovedPoints = Perm32Encoding.nMovedPoints(long2, long1, long0)
  def movedPoints = Perm32Encoding.movedPoints(long2, long1, long0)
  def largestMovedPoint = NNOption(Perm32Encoding.largestMovedPoint(long2, long1, long0))
  def smallestMovedPoint = NNOption(Perm32Encoding.smallestMovedPoint(long2, long1, long0))

  def isValidPerm32 = true
  def toPerm32 = this

}

object Perm32 extends PermCompanion {

  def movedPointsUpperBound = Perm32Encoding.supportMaxElement

  def fromImagesAndHighSupportMax(images: Array[Int], supportMax: Int): Perm32 =
    Perm32Encoding.fromImages(images, supportMax)

  def fromHighSupportAndImageFun(support: Set[Int], imageFun: Int => Int, supportMax: Int): Perm32 =
    Perm32Encoding.fromSupportAndImageFun(support, imageFun)

}
