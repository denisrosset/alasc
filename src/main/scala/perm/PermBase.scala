package com.faacets
package perm

import scala.util.Random

trait PermElement[E <: PermElement[E]] extends Any with FiniteElement[E] {
  def size: Int
  def image(k: Domain): Domain
  def images: ArrayDomain1 /** Images of 1..n under permutation, is one-based */

  protected[perm] def images0: ArrayDomain0 /** Images of 0..n-1 under permutation, is zero-based */
  def compare(that: E): Int
  def explicit: Perm

  def domain: Iterator[Domain] = (0 until size).toIterator.map(Domain.zeroBased(_))
  def isDefinedAt(k: Domain) = (k.zeroBased >= 0 && k.zeroBased < size)

  def cycle[P](start: Domain): List[Domain] = {
    def walk(el: Domain): List[Domain] =
      if (el == start)
        Nil
      else
        el :: walk(image(el))
    start :: walk(image(start))
  }

  def cycles: List[List[Domain]] = {
    var checked = scala.collection.mutable.BitSet(size)
    var i = Domain(size)
    var cycleList = List.empty[List[Domain]]
    while (i >= Domain(1)) {
      if(!checked(i.zeroBased)) {
        def visit(e: Domain, stopWhen: Domain): List[Domain] = {
          checked(e.zeroBased) = true
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
      i = Domain(i.value - 1)
    }
    cycleList.sortBy(_.head)
  }
}

trait PermGroup[E <: PermElement[E], G <: PermGroup[E, G]] extends Any with FiniteGroup[E, G] {
  def degree: Int
  def domain = (0 until degree).toIterator.map(Domain.zeroBased(_))
}

