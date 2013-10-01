package net.alasc

import scala.annotation.tailrec
import scala.util.Random

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

  def contains(k: Dom) = isDefinedAt(k)

  def updated(newGens: Iterable[F], allGens: Iterable[F]): Transversal[F]  /** Returns a new transversal extended with s added to its generators. */
  def conjugatedBy(g: F): Transversal[F]
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
