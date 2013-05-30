package net.alasc
package bsgs

import scala.language.higherKinds

import scala.annotation.tailrec
import scala.util.Random

trait TransLike[E <: PermElement[E]] extends PartialFunction[Dom, (E, E)] with Iterable[(Dom, (E, E))] {
  type ElementPair = (E, E)
  def beta: Dom /** Element for which the transversal is defined. */
  def u(b: Dom) = apply(b)._1
  def uinv(b: Dom) = apply(b)._2
  // we do not inherit from Map[Dom, UnderlyingElement], but define
  // *some* of its methods
  def contains(k: Dom) = isDefinedAt(k)
  def keysIterator: Iterator[Dom] = iterator.map(_._1)
  def valuesIterator: Iterator[ElementPair] = iterator.map(_._2)
  /** Returns a random element of the transversal. */
  def random(implicit gen: Random): (Dom, ElementPair) = {
    val num = gen.nextInt(size)
    iterator.drop(num).next()
  }
  /** Checks the sanity of the transversal. */
  def assertValid {
    for ((b, (ub, uinvb)) <- iterator)
      assert(ub.image(beta) == b && uinvb.image(b) == beta)
  }
  def updated(newGens: Iterable[E], allGens: Iterable[E]): TransLike[E]  /** Returns a new transversal extended with s added to its generators. */
  def mapValues[F <: PermElement[F]](f: E => F): TransLike[F]
  def builder: TransBuilderLike
}

trait Trans[T[E <: PermElement[E]] <: Trans[T, E], E <: PermElement[E]] extends TransLike[E] {
  def updated(newGens: Iterable[E], allGens: Iterable[E]): T[E]  /** Returns a new transversal extended with s added to its generators. */
  def builder: TransBuilder[T]
  def mapValues[F <: PermElement[F]](f: E => F): Trans[T, F]
}

trait TransBuilderLike {
  def empty[E <: PermElement[E]](beta: Dom, id: E): TransLike[E]
}

trait TransBuilder[T[E <: PermElement[E]] <: Trans[T, E]] extends TransBuilderLike {
  def empty[E <: PermElement[E]](beta: Dom, id: E): T[E]
}
