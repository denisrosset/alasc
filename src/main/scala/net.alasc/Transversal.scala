/*
# Transversals #
*/
package net.alasc

import scala.annotation.tailrec
import scala.util.Random
import scala.collection.immutable.TreeMap

class TEntryChain[F <: Finite[F]](val chain: List[TEntry[F]]) extends AnyVal {
  def compute: F = chain.map(_.u).reduce(_*_)
  def action(k: Dom) = (k /: chain) {
    case (el, te) => te.uAction.image(el)
  }
}

object TEntryChain {
  def empty[F <: Finite[F]]: TEntryChain[F] = new TEntryChain[F](Nil)
}

case class TEntry[F <: Finite[F]](u: F, uinv: F, action: Action[F], isIdentity: Boolean) {
  lazy val uAction = action.toPerm(u)
  def *(tec: TEntryChain[F]): TEntryChain[F] = isIdentity match {
    case true => tec
    case false => new TEntryChain(this :: tec.chain)
  }
}

object TEntry {
  def apply[F <: Finite[F]](u: F, uinv: F, action: Action[F]): TEntry[F] = TEntry(u, uinv, action, u.isIdentity)
}

trait GenTransversal {
  def builder: TransversalBuilder
  def beta: Dom /** Element for which the transversal is defined. */
  def contains(k: Dom): Boolean
  def isDefinedAt(k: Dom): Boolean
}

trait Transversal[F <: Finite[F]] extends ReadOnlyMap[Dom, TEntry[F]] with GenTransversal {
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

trait TransversalImpl[F <: Finite[F]] extends Transversal[F] with GenTransversalImpl with ReadOnlyMapImpl[Dom, TEntry[F]] {
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
