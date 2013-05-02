package com.faacets
package perm

import scala.util.Random
import scala.reflect.ClassTag

trait FiniteElement[E <: FiniteElement[E]] extends Any {
  def compatible(that: E): Boolean
  def inverse: E
  def *(that: E): E
  def isIdentity: Boolean
  def equal(that: E): Boolean
}

trait FiniteGroup[E <: FiniteElement[E]] extends Any {
  def directCopies(n: Int)(implicit c: ClassTag[E]) = new wreath.BaseGroup[FiniteGroup[E], E](this, n)
  def elementClassTag: ClassTag[E] = ClassTag[E](identity.getClass)
  def identity: E
  def order: BigInt
  def compatible(e: E): Boolean
  def contains(e: E): Boolean
  def random(implicit gen: Random): E
  def elements: Iterator[E]
  def generators: Iterator[E]
}
