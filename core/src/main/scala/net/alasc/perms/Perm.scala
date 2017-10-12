package net.alasc.perms

import scala.util.Random

import spire.algebra.Order
import net.alasc.perms.internal.{GenPrm, Prm}
import net.alasc.util.NNOption
import internal.syntax._
import net.alasc.algebra.PermutationAction
import net.alasc.finite.FaithfulPermutationActionBuilder

final class Perm(val p: Prm) { lhs =>

  require(p.length == 0 || p(p.length-1) != p.length-1 )

  override def toString =
    if (p.length == 0) "Perm.id" else "Perm" + p.toCycles.string

  override def hashCode = GenPrm.hash(p)

  override def equals(that: Any) = that match {
    case rhs: Perm => GenPrm.equ.eqv(p, rhs.p)
    case _ => false
  }

  def image(preimage: Int): Int = p.image(preimage)
  def invImage(image: Int): Int = p.invImage(image)

  def isId: Boolean = p.isId
  def inverse = new Perm(p.inverse)

  def largestMovedPoint: NNOption = new NNOption(p.largestMovedPoint)
  def smallestMovedPoint: NNOption = new NNOption(p.smallestMovedPoint)
  def movedPoints: Set[Int] = p.movedPoints
  def nMovedPoints: Int = p.nMovedPoints

  def apply(cycle: Int*): Perm = new Perm(p |+| Prm.fromCycle(cycle: _*))
  def apply(cycle: String): Perm = new Perm(p |+| Prm.fromCycle(cycle))

  def |+|(rhs: Perm): Perm = new Perm(lhs.p |+| rhs.p)

  def toCycles: Cycles = Cycles.fromPerm(lhs)

}

object Perm {

  implicit val algebra: PermAlgebra.type = PermAlgebra

  implicit object faithfulPermutationActionBuilder extends FaithfulPermutationActionBuilder[Perm] {
    override def toString = "Perm.faithfulPAB"
    def apply(generators: Iterable[Perm]): PermutationAction[Perm] = algebra
  }

  /** Identity permutation */
  val id = new Perm(Prm.id)

  /** Constructs a permutation from an array of images. The array is copied.
    *
    * @param images     Array of images representing the permutation
    */

  def fromImages(images: Array[Int]): Perm = new Perm(Prm.fromImages(images))

  def fromImages(images: Seq[Int]): Perm = images match {
    case wa: scala.collection.mutable.WrappedArray[Int] => fromImages(wa.array)
    case _ => fromImages(images.toArray)
  }

  /** Constructs a permutation that acts on 0..size-1 according to the given image function. */
  def fromImageFun(size: Int, imageFun: Int => Int): Perm = new Perm(Prm.fromImageFun(size, imageFun))

  def fromSupportAndImageFun(support: Set[Int], imageFun: Int => Int): Perm =
    new Perm(Prm.fromSupportAndImageFun(support, imageFun))

  /** Constructs a permutation from a cycle given as a variable number of arguments. */
  def apply(cycle: Int*): Perm = new Perm(Prm.fromCycle(cycle: _*))

  /** Constructs a permutation from a cycle represented as a string using the domain
    * 0..9, A..Z. */
  def apply(cycle: String): Perm = new Perm(Prm.fromCycle(cycle))

  /** Constructs a permutation which acts according to the given map. */
  def fromMap(map: Map[Int, Int]): Perm = new Perm(Prm.fromMap(map))

  def transposition(i: Int, j: Int): Perm = new Perm(Prm.transposition(i, j))

  def sorting[T:Order](seq: Seq[T]): Perm = new Perm(Prm.sorting(seq))

  def random(size: Int)(implicit gen: Random): Perm = new Perm(Prm.random(size))

}
