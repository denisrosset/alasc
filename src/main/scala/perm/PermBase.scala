package com.faacets
package perm

import scala.util.Random

trait PermElementLike extends Any {
  def size: Int
  def image(k: Dom): Dom
  def images: DomArray
  def invImage(k: Dom): Dom
  def explicit: Perm
  def domain: Iterator[Dom] = (0 until size).toIterator.map(Dom._0(_))
  def cycle[P](start: Dom): List[Dom] = {
    def walk(el: Dom): List[Dom] =
      if (el == start)
        Nil
      else
        el :: walk(image(el))
    start :: walk(image(start))
  }

  def cycles: List[List[Dom]] = {
    import Dom.IntOrder._
    var checked = scala.collection.mutable.BitSet(size)
    var i = Dom(size)
    var cycleList = List.empty[List[Dom]]
    while (i >= Dom(1)) {
      if(!checked(i._0)) {
        def visit(e: Dom, stopWhen: Dom): List[Dom] = {
          checked(e._0) = true
          if (image(e) == stopWhen)
            e :: Nil
          else
            e :: visit(image(e), stopWhen)
        }
        val cycle = visit(i, i)
        val mn = cycle.min
        val (bef, aft) = cycle.span(_ != mn)
        cycleList = (aft ++ bef) :: cycleList
      }
      i = Dom._0(i._0 - 1)
    }
    cycleList.sortBy(_.head)
  }
  def isDefinedAt(k: Dom) = (k._0 >= 0 && k._0 < size)
  def intCompare(that: PermElementLike) = {
    import Dom.IntOrder._
    val firstNotEqual = domain.find(k => image(k) != that.image(k))
    firstNotEqual match {
      case None => 0
      case Some(k) if image(k) <= that.image(k) => -1
      case _ => 1
    }
  }
}
trait PermElement[E <: PermElement[E]] extends Any with PermElementLike with FiniteElement[E] {
}

trait PermGroup[E <: PermElement[E]] extends Any with FiniteGroup[E] {
  def degree: Int
  def domain = (0 until degree).toIterator.map(Dom._0(_))
  def fromExplicit(p: Perm): Option[E]
}
