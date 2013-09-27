package net

import scala.language.implicitConversions

package object alasc {
  /* Cartesian product of traversable. */
  def combine[A](xs: Traversable[Traversable[A]]): Seq[Seq[A]] =
    xs.foldLeft(Seq(Seq.empty[A])){
      (x, y) => for (a <- x.view; b <- y) yield a :+ b }

  def combineList[A](xs: Traversable[Traversable[A]]): Seq[List[A]] =
    xs.foldLeft(Seq(List.empty[A])){
      (x, y) => for (a <- x.view; b <- y) yield a :+ b }

  def bind2sub(N: Seq[BigInt], i: BigInt): Seq[BigInt] =
    N.scanLeft((BigInt(0), i)) { case ((rem, quot), n) => (quot % n, quot / n) }.map(_._1).tail

  def bsub2ind(N: Seq[BigInt], I: Seq[BigInt]): BigInt =
    (N zip I).foldLeft((BigInt(0), BigInt(1))) { case ((tot, mul), (n, i)) => (tot + mul * i, mul * n) }._1

  def ind2sub(N: Seq[Int], i: Int): Seq[Int] =
    N.scanLeft((0, i)) { case ((rem, quot), n) => (quot % n, quot / n) }.map(_._1).tail

  def sub2ind(N: Seq[Int], I: Seq[Int]): Int =
    (N zip I).foldLeft((0, 1)) { case ((tot, mul), (n, i)) => (tot + mul * i, mul * n) }._1

  type Predicate[F <: FiniteElement[F]] = (F => Boolean)

  import scala.annotation.elidable
  import scala.annotation.elidable._

  @elidable(ASSERTION)
  def require_(requirement: Boolean) {
    if (!requirement)
      throw new java.lang.AssertionError("assumption failed")
  }
  @elidable(ASSERTION)	
  @inline final def require_(requirement: Boolean, message: => Any) {
    if (!requirement)
      throw new IllegalArgumentException("requirement failed: "+ message)
  }
}
