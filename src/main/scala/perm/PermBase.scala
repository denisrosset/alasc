package com.faacets
package perm

import scala.util.Random

trait PermElement[E <: PermElement[E]] extends Any with FiniteElement[E] {
  def size: Int
  def image(k: Dom): Dom
  def invImage(k: Dom): Dom
  def images: ArrayDom1 /** Images of 1..n under permutation, is one-based */

  protected[perm] def images0: ArrayDom0 /** Images of 0..n-1 under permutation, is zero-based */
  def compare(that: E): Int
  def explicit: Perm

  def domain: Iterator[Dom] = (0 until size).toIterator.map(Dom._0(_))
  def isDefinedAt(k: Dom) = (k._0 >= 0 && k._0 < size)

  def cycle[P](start: Dom): List[Dom] = {
    def walk(el: Dom): List[Dom] =
      if (el == start)
        Nil
      else
        el :: walk(image(el))
    start :: walk(image(start))
  }

  def cycles: List[List[Dom]] = {
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
}

trait PermGroup[E <: PermElement[E]] extends Any with FiniteGroup[E] {
  def degree: Int
  def domain = (0 until degree).toIterator.map(Dom._0(_))
  def fromExplicit(p: Perm): Option[E]
}
