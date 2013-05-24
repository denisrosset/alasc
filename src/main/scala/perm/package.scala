package com.faacets

import scala.language.implicitConversions

package object perm {
  type Base = Seq[Dom]
  implicit class RichDomArray(da: DomArray) extends IndexedSeq[Dom] {
    def length = da.length
    def apply(k: Int) = da(k)
  }
  /* Cartesian product of traversable. */
  def combine[A](xs: Traversable[Traversable[A]]): Seq[Seq[A]] =
    xs.foldLeft(Seq(Seq.empty[A])){
      (x, y) => for (a <- x.view; b <- y) yield a :+ b }

  def combineList[A](xs: Traversable[Traversable[A]]): Seq[List[A]] =
    xs.foldLeft(Seq(List.empty[A])){
      (x, y) => for (a <- x.view; b <- y) yield a :+ b }

//  def leaveInvariant[E <: PermElement[E], D](s: Seq[D])(e: E) =
//    s.sameElements(s.indices.map(i => s(e.image(Dom._0(i))._0)))
}
