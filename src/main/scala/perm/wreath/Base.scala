package com.faacets
package perm
package wreath

import scala.util.Random
import scala.reflect.ClassTag

/** Represents the base group of a wreath product group.
  * 
  * It is the direct product of a group G n times with itself.
  */
class BaseGroup[A <: FiniteGroup[F], F <: FiniteElement[F] : ClassTag](a: A, n: Int) extends
    FiniteGroup[BaseElement[F]] {
  override def toString = "" + n + " copies of " + a.toString
  def compatible(e: BaseElement[F]) = e.arr.size == n
  def contains(e: BaseElement[F]) = {
    require_(compatible(e))
    e.arr.forall(a.contains(_))
  }
  def iteratorOverCopies(d: Int): Iterator[List[F]] = d match {
    case 0 => List(List.empty[F]).iterator
    case _ => for {
      f <- a.elements
      rest <- iteratorOverCopies(d - 1)
    } yield (f :: rest)
  }
  def elements = iteratorOverCopies(n).map(list => new BaseElement(list.toArray))
  def identity = new BaseElement(Array.tabulate(n)(i => a.identity))
  def generators = for {
    k <- (0 until n).iterator
    g <- a.generators
  } yield new BaseElement(identity.arr.updated(k, g))
  def order = a.order.pow(n)
  def random(implicit gen: Random) = new BaseElement(Array.tabulate(n)(i => a.random))
}

class BaseElement[F <: FiniteElement[F] : ClassTag](val arr: Array[F]) extends FiniteElement[BaseElement[F]] {
  override def toString = arr.mkString("(",",",")")
  def apply(k: Dom) = arr(k._0)
  def *(that: BaseElement[F]) = {
    val newArr = new Array[F](arr.size)
    for (i <- 0 until arr.size) newArr(i) = arr(i)*that.arr(i)
    new BaseElement(newArr)
  }
  def compatible(that: BaseElement[F]) = arr.size == that.arr.size
  def equal(that: BaseElement[F]) = (0 until arr.size).forall( i => arr(i).equal(that.arr(i)))
  def inverse = {
    val newArr = new Array[F](arr.size)
    for (i <- 0 until arr.size) newArr(i) = arr(i).inverse
    new BaseElement(newArr)
  }
  def isIdentity = (0 until arr.size).forall( i => arr(i).isIdentity )
  def **![P <: PermElement[P]](p: P) = {
    val newArr = new Array[F](arr.size)
    for (i <- 0 until arr.size) newArr(i) = arr(p.image(Dom._0(i))._0)
    new BaseElement(newArr)
  }
  def **[P <: PermElement[P]](p: P) = {
    val newArr = new Array[F](arr.size)
    for (i <- 0 until arr.size) newArr(p.image(Dom._0(i))._0) = arr(i)
    new BaseElement(newArr)
  }
}
