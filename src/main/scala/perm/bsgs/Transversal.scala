package com.faacets
package perm
package bsgs

import scala.annotation.tailrec
import scala.util.Random
import scala.language.higherKinds
import scala.language.existentials

trait TransversalFactory[T <: Transversal[T, E], E <: PermElement[E]] {
  def empty(beta: Dom, id: E): T
}

trait TransversalLike[E <: PermElement[E]] extends PartialFunction[Dom, (E, E)] with Iterable[(Dom, (E, E))] {
  type ElementPair = (E, E)
  def beta: Dom /** Element for which the transversal is defined. */
  def u(b: Dom) = apply(b)._1
  def uinv(b: Dom) = apply(b)._2
  // we do not inherit from Map[Dom, UnderlyingElement], but define
  // *some* of its methods
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
}

trait Transversal[T <: Transversal[T, E], E <: PermElement[E]] extends TransversalLike[E] {
  def addingGenerator(s: E): T  /** Returns a new transversal extended with s added to its generators. */
}
