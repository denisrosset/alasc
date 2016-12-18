package net.alasc.bsgs

import scala.reflect.ClassTag

import spire.algebra.{Eq, Group}
import spire.math.SafeLong
import spire.util.Opt
import spire.syntax.action._
import spire.syntax.group._
import spire.syntax.eq._

import net.alasc.algebra.{BigIndexedSeq, PermutationAction}
import net.alasc.domains.Partition
import net.alasc.finite._
import net.alasc.perms.{FaithfulPermRep, Perm}

/** Algorithms for a group with a (family of) faithful permutation actions, of type A. */
trait GrpChainFaithfulPermutationAction[G, A <: PermutationAction[G]] extends GrpChainGroup[G] with GrpPermutationAction[G, A] {

  import GrpChain.{commonAction, extractGrpChain, forceAction}

  type GG = GrpChain[G, F] forSome { type F <: A with Singleton }

  /** Returns a faithful permutation action for the group expressed by the given generators. */
  def faithfulAction(generators: IndexedSeq[G]): A

  def trivial: GG = {
    val action = faithfulAction(IndexedSeq.empty[G])
    implicit def ia: action.type = action
    new GrpChainExplicit[G, action.type](Term[G, action.type], Opt(IndexedSeq.empty[G]))
  }

  def fromGenerators(generators: IndexedSeq[G], action: A, baseGuideOpt: Opt[BaseGuide]): GrpChain[G, action.type] = {
    implicit def ia: action.type = action
    val chain = BuildChain.fromGenerators[G, action.type](generators, baseGuideOpt)
    new GrpChainExplicit[G, action.type](chain, Opt(generators))
  }

  def fromGenerators(generators: IndexedSeq[G], action: A): GrpChain[G, action.type] =
    fromGenerators(generators, action, Opt.empty[BaseGuide])

  def fromGenerators(generators: IndexedSeq[G]): GG = {
    val action = faithfulAction(generators)
    fromGenerators(generators, action)
  }

  // fromGeneratorsAndOrder

  def fromGeneratorsAndOrder(generators: IndexedSeq[G], order: SafeLong,
                             action: A, baseGuideOpt: Opt[BaseGuide]): GrpChain[G, action.type] = {
    implicit def ia: action.type = action
    val chain = BuildChain.fromGeneratorsAndOrder[G, action.type](generators, order, baseGuideOpt)
    new GrpChainExplicit[G, action.type](chain, Opt(generators))
  }

  def fromGeneratorsAndOrder(generators: IndexedSeq[G], order: SafeLong, action: A): GrpChain[G, action.type] =
    fromGeneratorsAndOrder(generators, order, action, Opt.empty[BaseGuide])

  def fromGeneratorsAndOrder(generators: IndexedSeq[G], order: SafeLong): GG = {
    val action = faithfulAction(generators)
    fromGeneratorsAndOrder(generators, order, action)
  }

  // fromGrp

  def compatibleAction(action: PermutationAction[G]): Opt[action.type <:< A]

  def fromGrp(grp: Grp[G]): GG = grp match {
    case gc: GrpChain[G, _] =>
      val action = gc.action
      compatibleAction(gc.action) match {
        case Opt(sub) => grp.asInstanceOf[GG]
        case _ => fromGeneratorsAndOrder(grp.generators, grp.order)
      }
    case _ => fromGeneratorsAndOrder(grp.generators, grp.order)
  }

  def fromGrp(grp: Grp[G], action: A): GrpChain[G, action.type] = {
    extractGrpChain(grp, action) match {
      case Opt(gc) => gc
      case _ => fromGeneratorsAndOrder(grp.generators, grp.order, action)
    }
  }

  def fromGrp(grp: Grp[G], action: A, baseGuideOpt: Opt[BaseGuide]): GrpChain[G, action.type] = baseGuideOpt match {
    case Opt(baseGuide) => extractGrpChain(grp, action) match {
      case Opt(gc) => gc.chainOpt match {
        case Opt(chain) if baseGuide.isSatisfiedBy(chain) => gc
        case _ => fromGeneratorsAndOrder(grp.generators, grp.order, action, baseGuideOpt)
      }
      case _ => fromGeneratorsAndOrder(grp.generators, grp.order, action, baseGuideOpt)
    }
    case _ => fromGrp(grp, action)
  }

  /** Returns the given group as a BSGS chain using the given action; the given baseguide is used only
    * if the BSGS chain has to be fully recomputed.
    */
  protected def fromGrpBaseHint(grp: Grp[G], action: A, baseGuideHintOpt: Opt[BaseGuide]): GrpChain[G, action.type] =
    extractGrpChain(grp, action) match {
      case Opt(gc) => gc
      case _ => fromGeneratorsAndOrder(grp.generators, grp.order, action, baseGuideHintOpt)
    }

  override def conjugatedBy(grp: Grp[G], h: G): GG =
    if (grp.contains(h)) fromGrp(grp) else {
      val action: A = faithfulAction(grp.generators :+ h)
      implicit val ia: action.type = action
      extractGrpChain(grp, action) match {
        case Opt(conj: GrpChainConjugated[G, action.type]) =>
          new GrpChainConjugated[G, action.type](conj.originalChain, conj.g |+| h, h.inverse |+| conj.gInv,
            Opt(conj.originalGenerators))
        case Opt(cg: GrpChain[G, action.type]) =>
          new GrpChainConjugated[G, action.type](cg.chain, h, h.inverse, Opt(cg.generators))
        case _ =>
          val hInv = h.inverse
          fromGeneratorsAndOrder(grp.generators.map(g => hInv |+| g |+| h), grp.order)
      }
    }

  def union(lhs: Grp[G], rhs: Grp[G]): GG =
    if (rhs.order > lhs.order) union(rhs, lhs) // ensure that lhs.order >= rhs.order
    else if (lhs.hasSubgroup(rhs)) fromGrp(lhs)
    else commonAction(lhs, rhs) match {
      case Opt(action) => fromGrp(GrpChain.union(forceAction(lhs, action), rhs.generators))
      case _ =>
        val action = faithfulAction(lhs.generators ++ rhs.generators)
        GrpChain.union(fromGrp(lhs, action), rhs.generators)
    }

  def intersect(lhs: Grp[G], rhs: Grp[G]): GG =
    if (rhs.order > lhs.order) intersect(rhs, lhs) // ensure lhs.order >= rhs.order
    else if (lhs.hasSubgroup(rhs)) fromGrp(rhs)
    else commonAction(lhs, rhs) match {
      case Opt(action) => fromGrp(GrpChain.intersect(forceAction(lhs, action), forceAction(rhs, action)))
      case _ =>
        val action = faithfulAction(lhs.generators ++ rhs.generators)
        GrpChain.intersect[G, action.type](fromGrp(lhs, action), fromGrp(rhs, action))
    }

  def leftCosetsBy(grp: Grp[G], subgrp: Grp[G]): LeftCosets[G, subgrp.type] =
    commonAction(grp, subgrp) match {
      case Opt(action) =>
        GrpChain.leftCosetsBy[G, action.type](forceAction(grp, action), subgrp, forceAction(subgrp, action))
      case _ =>
        val grp1 = fromGrp(grp)
        val action = grp1.action
        val grp2 = forceAction(grp1, action)
        GrpChain.leftCosetsBy[G, action.type](grp2, subgrp, fromGrp(subgrp, action))
    }

  def rightCosetsBy(grp: Grp[G], subgrp: Grp[G]): RightCosets[G, subgrp.type] = leftCosetsBy(grp, subgrp).inverse

  def someStabilizerTransversal(grp: Grp[G], action: A): Opt[(GrpChain[G, action.type], Transversal[G, action.type])] =
    GrpChain.someStabilizerTransversal[G, action.type](fromGrp(grp, action))

  def stabilizer(grp: Grp[G], action: A, p: Int): GrpChain[G, action.type] =
    GrpChain.stabilizer[G, action.type](fromGrpBaseHint(grp, action, Opt(BaseGuideSingle(p))), p)

  def stabilizerTransversal(grp: Grp[G], action: A, p: Int): (GrpChain[G, action.type], Transversal[G, action.type]) =
    GrpChain.stabilizerTransversal[G, action.type](fromGrpBaseHint(grp, action, Opt(BaseGuideSingle(p))), p)

  def pointwiseStabilizer(grp: Grp[G], action: A, set: Set[Int]): GrpChain[G, action.type] =
    GrpChain.pointwiseStabilizer[G, action.type](fromGrpBaseHint(grp, action, Opt(BaseGuideSet(set))), set)

  def subgroupFor[F <: A with Singleton](grp: Grp[G], action: F, definition: SubgroupDefinition[G, F]): GrpChain[G, F] = {
    implicit def ia: F = action
    val guidedChain = extractGrpChain(grp, action: F) match {
      case Opt(grpChain) => grpChain match {
        case lhs: GrpChainConjugated[G, F] =>
          import lhs.{g, gInv, originalChain}
          val mut = originalChain.mutableChain
          mut.conjugate(g, gInv)
          definition.baseGuideOpt match {
            case Opt(baseGuide) => baseChange.changeBase(mut, baseGuide)
            case _ =>
          }
          mut.toChain()
        case lhs: GrpChain[G, F] => BuildChain.fromChain[G, F, F](lhs.chain, definition.baseGuideOpt)
      }
      case _ => BuildChain.fromGeneratorsAndOrder[G, F](grp.generators, grp.order, definition.baseGuideOpt)
    }
    val subChain = SubgroupSearch.subgroupSearch(definition, guidedChain).toChain()
    new GrpChainExplicit[G, F](subChain, Opt.empty[IndexedSeq[G]])
  }

  def fixingPartition(grp: Grp[G], action: A, partition: Partition): GrpChain[G, action.type] =
    subgroupFor[action.type](grp, action, net.alasc.bsgs.FixingPartition[G, action.type](partition)(implicitly, action))

  def setwiseStabilizer(grp: Grp[G], action: A, set: Set[Int]): GG =
    subgroupFor[action.type](grp, action, SetwiseStabilizer[G, action.type](set)(implicitly, action))

  def subgroupFor(grp: Grp[G], action: A, backtrackTest: (Int, Int) => Boolean, predicate: Perm => Boolean): GrpChain[G, action.type] =
    subgroupFor[action.type](grp, action, SubgroupDefinition[G, action.type](backtrackTest, g => predicate(action.toPerm(g)))(action))

  def lexElements(grp: Grp[G], action: A): Opt[BigIndexedSeq[G]] = {
    val cg = fromGrp(grp)
    val n = action.largestMovedPoint(grp.generators).getOrElseFast(0) + 1
    Opt(new GrpChain.LexElements[G, action.type](fromGrp(grp, action, Opt(BaseGuideLex(n)))))
  }

  def base(grp: Grp[G], action: A): Opt[Seq[Int]] = fromGrp(grp, action) match {
    case cj: GrpChainConjugated[G, action.type] => Opt(cj.originalChain.base.map(action.actr(_, cj.g)))
    case cg => Opt(cg.chain.base)
  }

}
