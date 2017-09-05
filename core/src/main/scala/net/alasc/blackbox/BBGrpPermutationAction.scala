package net.alasc.blackbox

import net.alasc.algebra.{BigIndexedSeq, PermutationAction}
import net.alasc.bsgs.FixingPartition
import net.alasc.finite._
import net.alasc.partitions.Partition
import net.alasc.perms.Perm
import net.alasc.syntax.group._
import spire.algebra.{Eq, Group}
import spire.util.Opt
import spire.syntax.cfor._
import spire.syntax.action._
import net.alasc.util.NNOption

import scala.reflect.ClassTag

class BBGrpPermutationAction[G](implicit
                                val classTag: ClassTag[G],
                                val group: Group[G],
                                val equ: Eq[G]
                               ) extends GrpPermutationAction[G] {

  type GG = BBGrp[G]

  def findSameAction[Q:PermutationAction](grp: Grp[G], action: PermutationAction[G], q: Q): Opt[G] =
    grp.iterator.find( g => action.hasSameAction(g, q) ) match {
      case Some(g) => Opt(g)
      case _ => Opt.empty[G]
    }

  def filter(grp: Grp[G], predicate: G => Boolean): GG =
    BBGrp.fromElements(BBGrp.fromGrp(grp).elements.filter(predicate))

  def lexElements(grp: Grp[G], action: PermutationAction[G]): Opt[BigIndexedSeq[G]] =
    if (!kernel(grp, action).isTrivial) Opt.empty[BigIndexedSeq[G]] else {
      val orderTC = net.alasc.lexico.lexPermutationOrder.LexPermutationOrder[G](implicitly, action)
      val ordering = orderTC.toOrdering
      val sortedElements = BBGrp.fromGrp(grp).elements.toIndexedSeq.sorted(ordering)
      Opt(BigIndexedSeq.wrap(sortedElements))
    }

  def fixingPartition(grp: Grp[G], action: PermutationAction[G], partition: Partition): GG =
    filter(grp, g => FixingPartition.partitionInvariantUnder(partition, action, g))

  def base(grp: Grp[G], action: PermutationAction[G]): Opt[Seq[Int]] = ??? //TODO implement

  def subgroupFor(grp: Grp[G], action: PermutationAction[G], backtrackTest: (Int, Int) => Boolean, predicate: (Perm) => Boolean): GG =
    filter(grp, g => predicate(action.toPerm(g)))

  def toPerm(grp: Grp[G], action: PermutationAction[G])(implicit algos: GrpGroup[Perm]): Grp[Perm] =
    algos.fromGenerators(grp.generators.map(g => action.toPerm(g)).toSet.toIndexedSeq)

  def kernel(grp: Grp[G], action: PermutationAction[G]): GG =
    filter(grp, g => !action.movesAnyPoint(g))

  def stabilizer(grp: Grp[G], action: PermutationAction[G], b: Int): GG =
    filter(grp, g => (action.actr(b, g) == b))

  def stabilizerTransversal(grp: Grp[G], action: PermutationAction[G], p: Int) = {
    val cosets = BBGrp.fromGrp(grp).elements.groupBy(g => action.actr(p, g))
    val stabilizer = BBGrp.fromElements(cosets(p))
    val transversalElements = cosets.mapValues(_.head)
    val transversal = new Transversal[G] {
      def elements = transversalElements.values
      def elementFor(g: G) = transversalElements(action.actr(p, g))
    }
    (stabilizer, transversal)
  }

  def someStabilizerTransversal(grp: Grp[G], action: PermutationAction[G]) = {
    val pOpt = GrpPermutationAction.findMovedPoint(grp)(action)
    pOpt match {
      case NNOption(p) => Opt(stabilizerTransversal(grp, action, p))
      case _ => Opt.empty[(Grp[G], Transversal[G])]
    }
  }

  def pointwiseStabilizer(grp: Grp[G], action: PermutationAction[G], set: Set[Int]): GG =
    filter(grp, g => set.forall(b => (action.actr(b, g) == b)))

  def setwiseStabilizer(grp: Grp[G], action: PermutationAction[G], set: Set[Int]): GG =
    filter(grp, g => (set.map(b => action.actr(b, g)) == set))

}
