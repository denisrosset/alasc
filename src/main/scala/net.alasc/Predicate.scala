package net.alasc

import scala.util.Random
import scala.annotation.tailrec

case class InvariantPredicate[D, E <: PermElement[E]](s: Seq[D]) extends Predicate[E] {
  def groups = s.zipWithIndex.groupBy(_._1).values.map( els => els.map(_._2) )
  def groupStrings = groups.map( _.map( _+1 )).map(_.mkString("["," ", "]"))
  def apply(e: E) =
    s.sameElements(s.indices.map(i => s(e.image(Dom._0(i))._0)))
}

class PredicateSubgroup[G <: PermGroup[E], E <: PermElement[E]](val g: G, val predicate: Predicate[E]) extends PermGroup[E] {
  def identity = g.identity
  def order = elements.size
  def degree = g.degree
  def compatible(e: E) = g.compatible(e)
  def contains(e: E) = {
    require_(compatible(e))
    predicate(e)
  }
  @tailrec final def randomElement(gen: Random) = {
    val e = g.random
    if (predicate(e))
      e
    else
      randomElement(gen)
  }
  def elements = g.elements.filter(predicate(_))
  def generators = elements.toSeq.filter(!_.isIdentity)
  def fromExplicit(p: Perm) = elements.find(_.explicit === p)
}
