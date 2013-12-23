/*
# Transversals #
*/
package net.alasc

import scala.annotation.tailrec
import scala.util.Random
import scala.collection.immutable.TreeMap

case class WithInverse[F <: FiniteElement[F]](u: F, uinv: F) { }

trait ReadOnlyMap[A, +B] extends PartialFunction[A, B] with Iterable[(A, B)] {
  def isDefinedAt(key: A): Boolean
  def apply(key: A): B
  def get(key: A) = apply(key)
  def iterator: Iterator[(A, B)]
  def keysIterator: Iterator[A] = iterator.map(_._1)
  def valuesIterator: Iterator[B] = iterator.map(_._2)
}

trait Transversal[F <: FiniteElement[F]] extends ReadOnlyMap[Dom, WithInverse[F]] {
  def builder: TransversalBuilder

  def action: Action[F]
  def beta: Dom /** Element for which the transversal is defined. */
  def identity: F

  def orbit: Set[Dom] = keysIterator.toSet

  def contains(k: Dom) = isDefinedAt(k)

  def updated(newGens: Iterable[F], allGens: Iterable[F]): Transversal[F]  /** Returns a new transversal extended with s added to its generators. */
  def conjugatedBy(f: F): Transversal[F] = conjugatedBy(f, f.inverse)
  def conjugatedBy(f: F, finv: F): Transversal[F]
  def mapValues[G <: FiniteElement[G]](f: F => G, gAction: Action[G]): Transversal[G]

  /** Returns a random element of the transversal. */
  def randomElement(gen: Random): F = {
    val num = gen.nextInt(size)
    valuesIterator.drop(num).next().u
  }

  /** Checks the sanity of the transversal. */
  def check {
    for (b <- keysIterator)
      assert(action(apply(b).u, beta) == b && action(apply(b).uinv, b) == beta)
  }
}

trait TransversalBuilder {
  def empty[F <: FiniteElement[F]](beta: Dom, identity: F, action: Action[F]): Transversal[F]
}

/*
## Implementation of `Transversal` using an explicit representation of group elements
*/

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
  import Dom.IntOrder._
  def empty[F <: FiniteElement[F]](beta: Dom, identity: F, action: Action[F]) = TransversalExplicit(beta, action, TreeMap((beta, WithInverse(identity, identity))))
}
