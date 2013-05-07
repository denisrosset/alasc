package com.faacets
package perm

import scala.util.Random
import scala.annotation.tailrec

trait Predicate extends Function1[PermElementLike, Boolean] with HasTeX { }

case class InvariantPredicate[D](s: Seq[D]) extends Predicate with HasTeX {
  def groups = s.zipWithIndex.groupBy(_._1).values.map( els => els.map(_._2) )
  def groupStrings = groups.map( _.map( _+1 )).map(_.mkString("["," ", "]"))
  def toTeX = TeX(groupStrings.mkString("_{"," ","}"))
  def apply(e: PermElementLike) =
    s.sameElements(s.indices.map(i => s(e.image(Dom._0(i))._0)))
}

class PredicateSubgroup[G <: PermGroup[E], E <: PermElement[E]](val g: G, val predicate: Predicate) extends PermGroup[E] with HasTeX {
  def toTeX = g.toTeX + predicate.toTeX
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
