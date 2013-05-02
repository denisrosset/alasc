package com.faacets
package perm

import scala.util.Random
import scala.annotation.tailrec

class PredicateSubgroup[G <: PermGroup[E], E <: PermElement[E]](val g: G, val predicate: E => Boolean) extends PermGroup[E] {
  def identity = g.identity
  def order = elements.size
  def degree = g.degree
  def compatible(e: E) = g.compatible(e)
  def contains(e: E) = {
    require_(compatible(e))
    predicate(e)
  }
  @tailrec final def random(implicit gen: Random) = {
    val e = g.random
    if (predicate(e))
      e
    else
      random
  }
  def elements = g.elements.filter(predicate(_))
  def generators = ???
  def fromExplicit(p: Perm) = elements.find(_.explicit.equal(p))
}
