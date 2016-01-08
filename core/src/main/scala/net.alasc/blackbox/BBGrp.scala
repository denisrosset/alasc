package net.alasc.blackbox

import scala.annotation.tailrec
import scala.util.Random

import spire.algebra.{Eq, Group}
import spire.syntax.group._

import net.alasc.finite._

class BBGrp[G](
    val generators: Iterable[G],
    val elements: Set[G]
  )(implicit
    val builder: BBGrpBuilder[G]
  ) extends Grp[G] {

  def iterator = elements.iterator

  def contains(g: G) = elements.contains(g)

  def order = elements.size

  def randomElement(random: Random): G = elements.iterator.drop(random.nextInt(elements.size)).next
  def union(rhs: Grp[G]) = builder.fromGenerators(generators ++ rhs.generators)

  def intersect(rhs: Grp[G]) = {
    val newElements = elements intersect rhs.iterator.toSet
    new BBGrp(newElements.filterNot(_.isId), newElements)
  }

  def leftCosetsBy(subgrp0: Grp.WithParent[this.type, G]): LeftCosets[G] = {
    @tailrec def rec(remaining: Set[G], transversal: Set[G]): Set[G] =
      if (remaining.isEmpty) transversal else {
        val g = remaining.head
        val cosetElements = subgrp0.iterator.map( h => g |+| h )
        rec(remaining diff cosetElements.toSet, transversal + g)
      }
    val transversal = rec(elements diff subgrp0.iterator.toSet, subgrp0.iterator.toSet)
    new LeftCosets[G] {
      type GG = BBGrp.this.type
      val grp: GG = BBGrp.this
      val subgrp: Grp.WithParent[GG, G] = subgrp0
      def iterator = transversal.iterator.map( g => new LeftCoset(g, subgrp) )
    }
  }

  def rightCosetsBy(subgrp0: Grp.WithParent[this.type, G]): RightCosets[G] = {
    @tailrec def rec(remaining: Set[G], transversal: Set[G]): Set[G] =
      if (remaining.isEmpty) transversal else {
        val g = remaining.head
        val cosetElements = subgrp0.iterator.map( h => h |+| g )
        rec(remaining diff cosetElements.toSet, transversal + g)
      }
    val transversal = rec(elements diff subgrp0.iterator.toSet, subgrp0.iterator.toSet)
    new RightCosets[G] {
      type GG = BBGrp.this.type
      val grp: GG = BBGrp.this
      val subgrp: Grp.WithParent[GG, G] = subgrp0
      def iterator = transversal.iterator.map( g => new RightCoset(g, subgrp) )
    }
  }

  def simplified = this

}

