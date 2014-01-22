package net.alasc

import scala.annotation.tailrec
import scala.util.Random
import scala.collection.immutable.TreeMap

/*
## Implementation of `Transversal` using an explicit representation of group elements
*/

case class TransversalExplicit[F <: Finite[F]](beta: Dom, action: Action[F], treeMap: TreeMap[Dom, WithInverse[F]]) extends Transversal[F] with TransversalLike[F] {

  def builder = TransversalExplicit

  def identity = apply(beta).u

  override def size = treeMap.size
  def isDefinedAt(b: Dom) = treeMap.isDefinedAt(b)
  def apply(b: Dom) = treeMap.apply(b)

  def iterator = treeMap.iterator
  override def keysIterator = treeMap.keysIterator
  override def valuesIterator = treeMap.valuesIterator

  def mapValues[G <: Finite[G]](f: F => G, gAction: Action[G]): TransversalExplicit[G] =
    TransversalExplicit(beta, gAction, TreeMap.empty[Dom, WithInverse[G]] ++ treeMap.mapValues( wi => WithInverse(f(wi.u), f(wi.uinv))))

  def updated(newGens: Iterable[F], allGens: Iterable[F]): TransversalExplicit[F] = {
    if (newGens.isEmpty)
      return this
    val newGensInv = newGens.map( g => (g, g.inverse) )
    var candidates = TreeMap.empty[Dom, WithInverse[F]] ++ (
      for ((s, sinv) <- newGensInv; b <- keysIterator; img = action(s, b) if !treeMap.contains(img) )
      yield (img, WithInverse(apply(b).u*s, sinv*apply(b).uinv)) )
    var newMap = treeMap ++ candidates
    if (newMap.isEmpty)
      return this
    val allGensInv = allGens.map( g => (g, g.inverse) )
    def checkCandidate(candidate: (Dom, WithInverse[F])): Boolean = {
      val (b, WithInverse(u, uinv)) = candidate
      for ((s, sinv) <- allGensInv; img = action(s, b) if !newMap.contains(img)) {
        val el = (img, WithInverse(u*s, sinv*uinv))
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
    TransversalExplicit(beta, action, newMap)
  }

  // implementation of AbstractTrans
  def conjugatedBy(f: F, finv: F): TransversalExplicit[F] = {
    TransversalExplicit(action(f, beta), action, treeMap.map { case (b, WithInverse(v,vinv)) => (action(f, b), WithInverse(finv*v*f, finv*vinv*f)) })
  }
}

object TransversalExplicit extends TransversalBuilder {
  def empty[F <: Finite[F]](beta: Dom, identity: F, action: Action[F]) = TransversalExplicit(beta, action, TreeMap((beta, WithInverse(identity, identity))))
}
