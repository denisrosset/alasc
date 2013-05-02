package com.faacets
package perm
package wreath

import scala.util.Random
import scala.reflect.ClassTag

/** Represents the base group of a wreath product group.
  * 
  * It is the direct product of a group G n times with itself.
  */
class InhBaseGroup[A <: FiniteGroup[F], F <: FiniteElement[F] : ClassTag](val a: Array[A], val n: Int) extends FiniteGroup[InhBaseElement[F]] {
  override def toString = "" + n + " copies of " + a.toString
  def compatible(e: InhBaseElement[F]) = e.arr.size == n
  def contains(e: InhBaseElement[F]) = {
    require_(compatible(e))
    (a zip e.arr).forall(Function.tupled( (gr, el) => gr.contains(el) ))
  }
  def iteratorOverCopies(d: Int): Iterator[List[F]] = d match {
    case 0 => List(List.empty[F]).iterator
    case _ => for {
      f <- a(a.size - d).elements
      rest <- iteratorOverCopies(d - 1)
    } yield (f :: rest)
  }
  def elements = iteratorOverCopies(n).map(list => new InhBaseElement(list.toArray))
  def identity = new InhBaseElement(a.map(_.identity))
  def generators = for {
    k <- (0 until n).iterator
    g <- a(k).generators
  } yield new InhBaseElement(identity.arr.updated(k, g))
  def order = a.foldLeft(BigInt(1))(_*_.order)
  def random(implicit gen: Random) = new InhBaseElement(a.map(_.random))
}

class InhBaseElement[F <: FiniteElement[F] : ClassTag](val arr: Array[F]) extends FiniteElement[InhBaseElement[F]] {
  override def toString = arr.mkString("(",",",")")
  def apply(k: Dom) = arr(k._0)
  def *(that: InhBaseElement[F]) = {
    require_(compatible(that))
    val newArr = new Array[F](arr.size)
    for (i <- 0 until arr.size) newArr(i) = arr(i)*that.arr(i)
    new InhBaseElement(newArr)
  }
  def compatible(that: InhBaseElement[F]) = arr.size == that.arr.size && (arr zip that.arr).forall(Function.tupled( (e1,e2) => e1.compatible(e2) ))
  def equal(that: InhBaseElement[F]) = {
    require_(compatible(that))
      (0 until arr.size).forall( i => arr(i).equal(that.arr(i)))
  }
  def inverse = {
    val newArr = new Array[F](arr.size)
    for (i <- 0 until arr.size) newArr(i) = arr(i).inverse
    val newEl = new InhBaseElement(newArr)
    assert(compatible(newEl))
    newEl
  }
  def isIdentity = (0 until arr.size).forall( i => arr(i).isIdentity )
  def **![P <: PermElement[P]](p: P) = {
    val newArr = new Array[F](arr.size)
    for (i <- 0 until arr.size) newArr(i) = arr(p.image(Dom._0(i))._0)
    val newEl = new InhBaseElement(newArr)
    assert(compatible(newEl))
    newEl
  }
  def **[P <: PermElement[P]](p: P) = {
    val newArr = new Array[F](arr.size)
    for (i <- 0 until arr.size) newArr(p.image(Dom._0(i))._0) = arr(i)
    val newEl = new InhBaseElement(newArr)
    assert(compatible(newEl))
    newEl
  }
}
