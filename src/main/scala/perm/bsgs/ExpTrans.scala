package com.faacets
package perm
package bsgs

import scala.collection.immutable.TreeMap

case class ExpTrans[E <: PermElement[E]](beta: Dom, map: TreeMap[Dom, (E, E)]) extends Trans[ExpTrans, E] {
  import Dom.IntOrder._

  def builder = ExpTransBuilder
  // implementation of PartialFunction
  def apply(b: Dom) = map.apply(b)

  def mapValues[F <: PermElement[F]](f: E => F): ExpTrans[F] = new ExpTrans(beta, TreeMap.empty[Dom, (F, F)] ++ map.mapValues( Function.tupled( (u, uinv) => (f(u), f(uinv)) )))
  // implementation of Iterable
  def isDefinedAt(b: Dom) = map.isDefinedAt(b)
  def iterator = map.iterator
  override def size = map.size // for speed reasons

  // implementation of AbstractTrans
  def updated(newGens: Iterable[E], allGens: Iterable[E]): ExpTrans[E] = {
    val newGensInv = newGens.map( g => (g, g.inverse) )
    var candidates = TreeMap.empty[Dom, (E, E)] ++ (
      for ((s, sinv) <- newGensInv; b <- keysIterator; img = s.image(b) if !map.contains(img) )
      yield (img, (u(b)*s, sinv*uinv(b))) )
    var newMap = map ++ candidates
    if (newMap.isEmpty)
      return this
    val allGensInv = allGens.map( g => (g, g.inverse) )
    def checkCandidate(candidate: (Dom, (E, E))): Boolean = {
      val (b, (u, uinv)) = candidate
      for ((s, sinv) <- allGensInv; img = s.image(b) if !newMap.contains(img)) {
        val el = (img, (u*s, sinv*uinv))
        candidates = candidates + el
        newMap = newMap + el
        return true
      }
      return false
    }
    while (!candidates.isEmpty) {
      val h = candidates.head
      while(checkCandidate(h)) { }
      candidates = candidates - h._1
    }
    ExpTrans(beta, newMap)
  }
}

object ExpTransBuilder extends TransBuilder[ExpTrans] {
  import Dom.IntOrder._
  def empty[E <: PermElement[E]](beta: Dom, id: E) = ExpTrans(beta, TreeMap((beta, (id, id))))
}
