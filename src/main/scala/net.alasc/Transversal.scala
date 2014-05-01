/*
# Transversals #
*/
package net.alasc

import scala.annotation.tailrec
import scala.util.Random
import scala.collection.immutable.TreeMap

case class TEntry[F <: Finite[F]](u: F, uinv: F)

trait GenTransversal {
  def builder: TransversalBuilder
  def beta: Dom /** Element for which the transversal is defined. */
  def contains(k: Dom): Boolean
  def isDefinedAt(k: Dom): Boolean
}

trait Transversal[F <: Finite[F]] extends ReadOnlyMapDom[TEntry[F]] with GenTransversal {
  def action: Action[F]
  def identity: F
  /** Returns a new transversal extended with s added to its generators. */
  def updated(newGens: Iterable[F], allGens: Iterable[F]): Transversal[F]  
  def conjugatedBy(f: F): Transversal[F]
  def conjugatedBy(f: F, finv: F): Transversal[F]
  def mapValues[G <: Finite[G]](f: F => G, gAction: Action[G]): Transversal[G]
  /** Returns a random element of the transversal. */
  def random(implicit gen: Random): F
  /** Checks the sanity of the transversal. */
  def check: Unit
  def orbitSet: Set[Dom]
}

trait GenTransversalImpl extends GenTransversal {
  def contains(k: Dom) = isDefinedAt(k)
}

trait TransversalImpl[F <: Finite[F]] extends Transversal[F] with GenTransversalImpl with ReadOnlyMapDomImpl[TEntry[F]] {
  def conjugatedBy(f: F): Transversal[F] = conjugatedBy(f, f.inverse)
  /** Returns a random element of the transversal. */
  def random(implicit gen: Random): F = {
    val num = gen.nextInt(size)
    valuesIterator.drop(num).next().u
  }
  def check {
    for (b <- keysIterator)
      assert(action(apply(b).u, beta) == b && action(apply(b).uinv, b) == beta)
  }
  def orbitSet: Set[Dom] = keysIterator.toSet
}

trait TransversalBuilder {
  def empty[F <: Finite[F]](beta: Dom, action: Action[F]): Transversal[F]
}
