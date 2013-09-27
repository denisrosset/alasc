package net.alasc

import scala.collection.immutable.TreeMap

case class TransversalExplicit[F <: FiniteElement[F]](beta: Dom, action: Action[F], treeMap: TreeMap[Dom, WithInverse[F]]) extends Transversal[F] {
  import Dom.IntOrder._

  def builder = TransversalExplicit

  def identity = apply(beta).u

  override def size = treeMap.size
  def isDefinedAt(b: Dom) = treeMap.isDefinedAt(b)
  def apply(b: Dom) = treeMap.apply(b)

  def iterator = treeMap.iterator
  override def keysIterator = treeMap.keysIterator
  override def valuesIterator = treeMap.valuesIterator

  def mapValues[G <: FiniteElement[G]](f: F => G, gAction: Action[G]): TransversalExplicit[G] =
    TransversalExplicit(beta, gAction, TreeMap.empty[Dom, WithInverse[G]] ++ treeMap.mapValues( wi => WithInverse(f(wi.u), f(wi.uinv))))

  def updated(newGens: Iterable[F], allGens: Iterable[F]): TransversalExplicit[F] = {
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
  def conjugatedBy(g: F): TransversalExplicit[F] = {
    val ginv = g.inverse
    TransversalExplicit(action(g, beta), action, treeMap.map { case (b, WithInverse(v,vinv)) => (action(g, b), WithInverse(ginv*v*g, ginv*vinv*g)) })
  }
}

object TransversalExplicit extends TransversalBuilder {
  import Dom.IntOrder._
  def empty[F <: FiniteElement[F]](beta: Dom, identity: F, action: Action[F]) = TransversalExplicit(beta, action, TreeMap((beta, WithInverse(identity, identity))))
}
