package net.alasc.blackbox

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

import spire.algebra.{Eq, Group, Order}
import spire.math.SafeLong
import spire.syntax.cfor._
import spire.syntax.group._
import spire.util.Opt

import net.alasc.algebra.{BigIndexedSeq, PermutationAction}
import net.alasc.bsgs.{FixingPartition, Transversal}
import net.alasc.partitions.Partition
import net.alasc.finite._
import net.alasc.perms.Perm
import net.alasc.syntax.group._

class BBGrpAlgos[G](implicit
                    val classTag: ClassTag[G],
                    val group: Group[G],
                    val equ: Eq[G]
  ) extends GrpGroup[G] with GrpPermutationAction[G] with GrpStructure[G] {

  type GG = BBGrp[G]

  def grpGroup = this

  def trivial: GG = new BBGrp(IndexedSeq.empty[G], Set(group.id))

  override def smallGeneratingSet(grp: Grp[G]): IndexedSeq[G] = {
    def computeOrder(gens: IndexedSeq[G]): SafeLong = SafeLong(Dimino[G](gens).length)
    GrpStructure.deterministicReduceGenerators(grp.generators, grp.order, computeOrder).getOrElseFast(grp.generators)
  }

  def fromElements(elements: Set[G]): GG = {
    val remaining = elements.to[collection.mutable.HashSet]
    val generators = ArrayBuffer.empty[G]
    val reconstructed = ArrayBuffer(Group[G].id)
    remaining -= Group[G].id
    while (remaining.nonEmpty) {
      val startRemove = reconstructed.length
      val g = remaining.head
      generators += g
      Dimino.runInduction(reconstructed, generators, generators.length - 1)
      cforRange(startRemove until reconstructed.length) { i => remaining -= reconstructed(i) }
    }
    new BBGrp[G](generators, elements)
  }

  @tailrec final protected def recElements(generatorsAndId: Set[G], elements: Set[G]): Set[G] = {
    val newElements = elements.flatMap(e => generatorsAndId.map(g => e |+| g))
    if (newElements.size == elements.size) elements else recElements(generatorsAndId, newElements)
  }

  def generateElements(generators: Iterable[G]): Set[G] =
    Dimino[G](generators.toIndexedSeq).toSet

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
    if (!kernel(grp, action).isTrivial) Opt.empty[BigIndexedSeq[G]] else {
      val orderTC = net.alasc.lexico.lexPermutationOrder.LexPermutationOrder[G](implicitly, action)
      val ordering = orderTC.toOrdering
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

  def toPerm(grp: Grp[G], action: PermutationAction[G])(implicit algos: GrpGroup[Perm]): Grp[Perm] =
    algos.fromGenerators(grp.generators.map(g => action.toPerm(g)).toSet.toIndexedSeq)

}
