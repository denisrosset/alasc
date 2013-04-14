package com.faacets

import scala.language.implicitConversions

package object perm {
  type Domain = Int
  type Base = Seq[Domain]

  /* Cartesian product of traversable. */
  def combine[A](xs: Traversable[Traversable[A]]): Seq[Seq[A]] =
    xs.foldLeft(Seq(Seq.empty[A])){
      (x, y) => for (a <- x.view; b <- y) yield a :+ b }

  implicit def domainAction(el: Domain) = new {
    def **[P <: PermutationGroup#Permutation](p: P) = p.image(el)
  }

  implicit def permutationOrdering[P <: PermutationGroup#Permutation]: Ordering[P] = {
    import scala.math.Ordering.Implicits._
    Ordering.fromLessThan(_.images < _.images)
  }
}
