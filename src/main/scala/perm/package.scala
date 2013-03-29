package com.faacets

package object perm {
  type Domain = Int
  type Base = Seq[Domain]

  /* Cartesian product of traversable. */
  def combine[A](xs: Traversable[Traversable[A]]): Seq[Seq[A]] =
    xs.foldLeft(Seq(Seq.empty[A])){
      (x, y) => for (a <- x.view; b <- y) yield a :+ b }

  implicit def empowerMyDomain(alpha: Domain) = new EmpoweredDomain(alpha)

  implicit def permutationOrdering[P <: Permutation[P]]: Ordering[P] = {
    import scala.math.Ordering.Implicits._
    Ordering.fromLessThan(_.images < _.images)
  }
}
