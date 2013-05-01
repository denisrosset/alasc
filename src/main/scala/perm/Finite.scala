package com.faacets
package perm

import scala.util.Random

trait FiniteElement[E <: FiniteElement[E]] extends Any {
  def compatible(that: E): Boolean
  def inverse: E
  def *(that: E): E
  def isIdentity: Boolean
  def equal(that: E): Boolean
}

trait FiniteGroup[E <: FiniteElement[E], G <: FiniteGroup[E, G]] extends Any {
  def identity: E
  def order: BigInt
  def compatible(e: E): Boolean
  def contains(e: E): Boolean
  def random(implicit gen: Random): E
  def elements: Iterator[E]
  def generators: Iterator[E]
}
