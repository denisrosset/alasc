package net.alasc.blackbox

import scala.annotation.tailrec

import spire.algebra.{Eq, Group, Order}
import spire.math.SafeLong
import spire.syntax.group._
import spire.util.Opt

import net.alasc.algebra.{BigIndexedSeq, PermutationAction}
import net.alasc.bsgs.{FixingPartition, Transversal}
import net.alasc.domains.Partition
import net.alasc.finite._
import net.alasc.perms.Perm.algebra
import net.alasc.perms.{Perm, PermGrpBuilder}

class BBGrpBuilder[G](implicit
    val group: Group[G],
    val equ: Eq[G]
  ) extends GrpBuilder[G] with PermutationActionGrpBuilder[G] {

  type GG = BBGrp[G]

  def trivial: GG = new BBGrp(IndexedSeq.empty[G], Set(group.id))

  def fromElements(elements: Set[G]): GG = {
    import scala.collection.mutable.{HashSet, Stack}
    val order = elements.size
    val remGenerators = elements.to[Stack]
    var generators = HashSet.empty[G]
    var checkElements = generateElements(generators)
    while (checkElements.size < order) {
      val g = remGenerators.pop()
      if (!elements.contains(g)) {
        generators += g
        checkElements = generateElements(generators)
      }
    }
    new BBGrp(generators.toIndexedSeq, elements)
  }

  def generateElements(generators: Iterable[G]): Set[G] = {
    @tailrec def rec(elements: Set[G]): Set[G] = {
      val newElements = generators
        .flatMap(g1 => elements.map(g2 => g1 |+| g2))
        .filterNot(elements.contains(_))
      if (newElements.isEmpty) elements else rec(elements ++ newElements)
    }
    rec(generators.toSet + Group[G].id)
  }

  def fromGenerators(generators: IndexedSeq[G]): GG = {
    new BBGrp(generators, generateElements(generators))
  }

  def fromGeneratorsAndOrder(generators: IndexedSeq[G], order: SafeLong): GG =
    fromGenerators(generators)

  def fromGrp(grp: Grp[G]): GG = grp match {
    case bb: BBGrp[G] => bb
    case _ => new BBGrp(grp.generators, grp.iterator.toSet)
  }

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

  def kernel(grp: Grp[G], action: PermutationAction[G]): GG =
    filter(grp, g => !action.movesAnyPoint(g))

  def lexElements(grp: Grp[G], action: PermutationAction[G]): Opt[BigIndexedSeq[G]] =
    if (kernel(grp, action).order == 1) Opt.empty[BigIndexedSeq[G]] else {
      val orderTC = net.alasc.lexico.lexPermutationOrder.LexPermutationOrder[G](implicitly, action)
      val ordering = Order.ordering(orderTC)
      val sortedElements = fromGrp(grp).elements.toIndexedSeq.sorted(ordering)
      Opt(BigIndexedSeq.wrap(sortedElements))
    }

  def filter(grp: Grp[G], predicate: G => Boolean): GG =
    fromElements(fromGrp(grp).elements.filter(predicate))

  def fixingPartition(grp: Grp[G], action: PermutationAction[G], partition: Partition): GG =
    filter(grp, g => FixingPartition.partitionInvariantUnder(partition, action, g))

  def stabilizer(grp: Grp[G], action: PermutationAction[G], b: Int): GG =
    filter(grp, g => (action.actr(b, g) == b))

  def someStabilizerTransversal(grp: Grp[G], action: PermutationAction[G]): Opt[(GG, Transversal[G, action.type])] = ???

  def stabilizerTransversal(grp: Grp[G], action: PermutationAction[G], b: Int): (GG, Transversal[G, action.type]) = ???

  def pointwiseStabilizer(grp: Grp[G], action: PermutationAction[G], set: Set[Int]): GG =
    filter(grp, g => set.forall(b => (action.actr(b, g) == b)))

  def setwiseStabilizer(grp: Grp[G], action: PermutationAction[G], set: Set[Int]): GG =
    filter(grp, g => (set.map(b => action.actr(b, g)) == set))

  def find[Q: Eq : Group](grp: Grp[G], actionG: PermutationAction[G], actionQ: PermutationAction[Q], q: Q): Opt[G] = ???

  def find(grp: Grp[G], actionG: PermutationAction[G], p: Perm): Opt[G] = ???

  def subgroupFor(grp: Grp[G], action: PermutationAction[G], backtrackTest: (Int, Int) => Boolean, predicate: (Perm) => Boolean): GG =
    filter(grp, g => predicate(action.toPerm(g)))

  def base(grp: Grp[G], action: PermutationAction[G]): Opt[Seq[Int]] = ???

  def toPerm(grp: Grp[G], action: PermutationAction[G])(implicit builder: PermGrpBuilder): Grp[Perm] =
    builder.fromGenerators(grp.generators.map(g => action.toPerm(g)).toSet.toIndexedSeq)

}

class PermBBGrpBuilder extends BBGrpBuilder[Perm] with PermGrpBuilder {

  override val equ = Perm.algebra

  override val group = Perm.algebra

  def setwiseStabilizer(grp: Grp[Perm], set: Set[Int]): BBGrp[Perm] = setwiseStabilizer(grp, Perm.algebra, set)

  def pointwiseStabilizer(grp: Grp[Perm], set: Set[Int]): BBGrp[Perm] = pointwiseStabilizer(grp, Perm.algebra, set)

  def stabilizerTransversal(grp: Grp[Perm], b: Int): (BBGrp[Perm], Transversal[Perm, Perm.algebra.type]) = stabilizerTransversal(grp, Perm.algebra, b)

  def someStabilizerTransversal(grp: Grp[Perm]): Opt[(BBGrp[Perm], Transversal[Perm, Perm.algebra.type])] = someStabilizerTransversal(grp, Perm.algebra)

  def stabilizer(grp: Grp[Perm], b: Int): BBGrp[Perm] = stabilizer(grp, Perm.algebra, b)

  def fixingPartition(grp: Grp[Perm], partition: Partition): BBGrp[Perm] = fixingPartition(grp, Perm.algebra, partition)

  def subgroupFor(grp: Grp[Perm], backtrackTest: (Int, Int) => Boolean, predicate: (Perm) => Boolean): BBGrp[Perm] =
    subgroupFor(grp, Perm.algebra, backtrackTest, predicate)

  def lexElements(grp: Grp[Perm]): BigIndexedSeq[Perm] = lexElements(grp, Perm.algebra).get

  def base(grp: Grp[Perm]): Seq[Int] = base(grp, Perm.algebra).get

  def find[Q:Eq:Group](grp: Grp[Perm], actionQ: PermutationAction[Q], q: Q): Opt[Perm] = find(grp, Perm.algebra, actionQ, q)

}