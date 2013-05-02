package com.faacets

import scala.language.implicitConversions

package object perm {
  type Base = Seq[Domain]
  type ArrayDomain0 = Array[Int]
  type ArrayDomain1 = Array[Int]
  /* Cartesian product of traversable. */
  def combine[A](xs: Traversable[Traversable[A]]): Seq[Seq[A]] =
    xs.foldLeft(Seq(Seq.empty[A])){
      (x, y) => for (a <- x.view; b <- y) yield a :+ b }

  def combineList[A](xs: Traversable[Traversable[A]]): Seq[List[A]] =
    xs.foldLeft(Seq(List.empty[A])){
      (x, y) => for (a <- x.view; b <- y) yield a :+ b }

//  implicit def asDomain(k: Int) = Domain(k)
}
