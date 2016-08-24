package net.alasc.algebra


import spire.algebra._

/** Type class for Permutation-like objects, where instances can be constructed
  * from arbitrary images.
  */
trait PermutationBuilder[P] extends Permutation[P] {

  def actl(p: P, k: Int) = actr(k, inverse(p))

  def transposition(i: Int, j: Int): P =
    fromMap(Map(i -> j, j -> i))

  def fromImages(images: Seq[Int]): P

  /** Returns a permutation from the given image array (the array is considered mutable and is copied if needed). */
  def fromImages(images: Array[Int]): P

  def fromSupportAndImageFun(support: Set[Int], image: Int => Int): P

  def fromImageFun(n: Int, image: Int => Int): P =
    fromSupportAndImageFun(Set(0 until n:_*), image)

  def fromMap(map: Map[Int, Int]): P =
    fromSupportAndImageFun(map.keySet, k => map.getOrElse(k, k))

  def sorting[T:Order](seq: Seq[T]): P = {
    import spire.compat._
    fromImages(seq.zipWithIndex.sortBy(_._1).map(_._2))
  }

  def random(size: Int)(implicit gen: scala.util.Random): P = {
    import spire.std.int._
    sorting(Seq.tabulate(size)(k => gen.nextInt))
  }

}

object PermutationBuilder {

  def apply[P: PermutationBuilder] = implicitly[PermutationBuilder[P]]

}
