package net

import scala.language.implicitConversions

package object alasc {
  type Base = Seq[Dom]
  /* Cartesian product of traversable. */
  def combine[A](xs: Traversable[Traversable[A]]): Seq[Seq[A]] =
    xs.foldLeft(Seq(Seq.empty[A])){
      (x, y) => for (a <- x.view; b <- y) yield a :+ b }

  def combineList[A](xs: Traversable[Traversable[A]]): Seq[List[A]] =
    xs.foldLeft(Seq(List.empty[A])){
      (x, y) => for (a <- x.view; b <- y) yield a :+ b }

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
