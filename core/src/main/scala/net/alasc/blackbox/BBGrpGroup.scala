package net.alasc.blackbox

import net.alasc.finite._
import spire.algebra.{Eq, Group}
import spire.math.SafeLong
import spire.syntax.group._
import net.alasc.syntax.group._
import scala.annotation.tailrec
import spire.syntax.eq._
import scala.reflect.ClassTag

import spire.util.Opt

class BBGrpGroup[G](implicit
                    val classTag: ClassTag[G],
                    val group: Group[G],
                    val equ: Eq[G]
                   ) extends GrpGroup[G] {

  type GG = BBGrp[G]

  def trivial: GG = new BBGrp(IndexedSeq.empty[G], Set(group.id))

  def generateElements(generators: Iterable[G]): Set[G] =
    Dimino[G](generators.toIndexedSeq).toSet

  def fromGenerators(generators: Seq[G]): GG = {
    new BBGrp(generators, generateElements(generators))
  }

  def fromGeneratorsAndOrder(generators: Seq[G], order: SafeLong): GG =
    fromGenerators(generators)

  def fromGrp(grp: Grp[G]): GG = BBGrp.fromGrp(grp)

  def union(x: Grp[G], y: Grp[G]): GG = fromGenerators(x.generators ++ y.generators)

  def intersect(x: Grp[G], y: Grp[G]): GG = {
    val newElements = fromGrp(x).elements intersect fromGrp(y).elements
    new BBGrp[G](newElements.filterNot(_.isId).toIndexedSeq, newElements)
  }

  def leftCosetsBy(grp0: Grp[G], subgrp0: Grp[G]): LeftCosets[G, subgrp0.type] = {
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
    new LeftCosets[G, subgrp0.type] {
      val grp = grp0
      val subgrp: subgrp0.type = subgrp0
      def iterator = transversal.iterator.map( g => new LeftCoset(g, subgrp0) )
      def inverse = rightCosetsBy(grp, subgrp)
    }
  }

  def rightCosetsBy(grp0: Grp[G], subgrp0: Grp[G]): RightCosets[G, subgrp0.type] = {
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
    new RightCosets[G, subgrp0.type] {
      val grp = grp0
      val subgrp : subgrp0.type = subgrp0
      def iterator = transversal.iterator.map( g => new RightCoset(g, subgrp0) )
      def inverse = leftCosetsBy(grp, subgrp)
    }
  }

  def areConjugate(grp: Grp[G], g1: G, g2: G): Boolean =
    fromGrp(grp).elements.exists(g => (g1 |+| g) === (g |+| g2))

  def areConjugate(grp: Grp[G], g1: G, g2: G, g2CentralizerSubgroup: Opt[Grp[G]]) = areConjugate(grp, g1, g2)

  def findConjugation(grp: Grp[G], g1: G, g2: G) =
    fromGrp(grp).elements.find(g => (g1 |+| g) === (g |+| g2)) match {
      case Some(g) => Opt(g)
      case None => Opt.empty[G]
    }

  def findConjugation(grp: Grp[G], g1: G, g2: G, g2CentralizerSubgroup: Opt[Grp[G]]) = findConjugation(grp, g1, g2)

  def centralizer(grp: Grp[G], g: G) =
    BBGrp.fromElements(fromGrp(grp).elements.filter(h => (g |+| h) === (h |+| g)))

}
