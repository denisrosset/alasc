package net.alasc.blackbox

import scala.annotation.tailrec

import spire.algebra.{Eq, Group}
import spire.math.SafeLong
import spire.syntax.group._

import net.alasc.finite._

class BBGrpBuilder[G](implicit
    val group: Group[G],
    val equ: Eq[G]
  ) extends GrpBuilder[G] {

  type GG = BBGrp[G]

  def trivial: GG = new BBGrp(Iterable.empty[G], Set(group.id))

  def fromGenerators(generators: Iterable[G]): GG = {
    @tailrec def rec(elements: Set[G]): Set[G] = {
      val newElements = generators
        .flatMap(g1 => elements.map(g2 => g1 |+| g2))
        .filterNot(elements.contains(_))
      if (newElements.isEmpty) elements else rec(elements ++ newElements)
    }
    new BBGrp(generators, rec(generators.toSet))
  }

  def fromGeneratorsAndOrder(generators: Iterable[G], order: SafeLong): GG =
    fromGenerators(generators)

  def fromGrp(grp: Grp[G]): GG = grp match {
    case bb: BBGrp[G] => bb
    case _ => new BBGrp(grp.generators, grp.iterator.toSet)
  }

  def union(x: Grp[G], y: Grp[G]): GG = fromGenerators(x.generators ++ y.generators)

  def intersect(x: Grp[G], y: Grp[G]): GG = {
    val newElements = fromGrp(x).elements intersect fromGrp(y).elements
    new BBGrp[G](newElements.filterNot(_.isId), newElements)
  }

  def leftCosetsBy(grp0: Grp[G], subgrp0: Grp[G]): LeftCosets[G] = {
    @tailrec def rec(remaining: Set[G], transversal: Set[G]): Set[G] =
      if (remaining.isEmpty) transversal else {
        val g = remaining.head
        val cosetElements = subgrp0.iterator.map( h => g |+| h )
        rec(remaining diff cosetElements.toSet, transversal + g)
      }
    val elements = grp0 match {
      case bb: BBGrp[G] => bb.elements
      case _ => grp0.iterator.toSet
    }
    val transversal = rec(elements diff subgrp0.iterator.toSet, subgrp0.iterator.toSet)
    new LeftCosets[G] {
      val grp = grp0
      val subgrp = subgrp0
      def iterator = transversal.iterator.map( g => new LeftCoset(g, subgrp0) )
    }
  }

  def rightCosetsBy(grp0: Grp[G], subgrp0: Grp[G]): RightCosets[G] = {
    @tailrec def rec(remaining: Set[G], transversal: Set[G]): Set[G] =
      if (remaining.isEmpty) transversal else {
        val g = remaining.head
        val cosetElements = subgrp0.iterator.map( h => h |+| g )
        rec(remaining diff cosetElements.toSet, transversal + g)
      }
    val elements = grp0 match {
      case bb: BBGrp[G] => bb.elements
      case _ => grp0.iterator.toSet
    }
    val transversal = rec(elements diff subgrp0.iterator.toSet, subgrp0.iterator.toSet)
    new RightCosets[G] {
      val grp = grp0
      val subgrp = subgrp0
      def iterator = transversal.iterator.map( g => new RightCoset(g, subgrp0) )
    }
  }

}
