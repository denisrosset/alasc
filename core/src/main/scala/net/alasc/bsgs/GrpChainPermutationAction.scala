package net.alasc.bsgs

import scala.reflect.ClassTag

import spire.algebra.{Eq, Group}
import spire.math.SafeLong
import spire.util.Opt
import spire.syntax.group._
import net.alasc.algebra.{BigIndexedSeq, PermutationAction}
import net.alasc.bsgs.MutableChain.Generic
import net.alasc.bsgs.internal.{Conjugation, GrpChainConjugated, GrpChainExplicit}
import net.alasc.partitions.Partition
import net.alasc.finite._
import net.alasc.perms.{FilterOrders, Perm}
import net.alasc.syntax.group._

/** Algorithms for groups for which faithful permutation actions can be obtained. */
abstract class GrpChainPermutationAction[G] extends GrpGroup[G] with GrpPermutationAction[G] with GrpStructure[G] {

  implicit def baseChange: BaseChange
  implicit def baseSwap: BaseSwap
  implicit def equ: Eq[G]
  implicit def classTag: ClassTag[G]
  implicit def group: Group[G]
  implicit def schreierSims: SchreierSims

  import GrpChain.{commonAction, extractGrpChain, inActionUnsafe}

  type GenericGC = GrpChain[G, _ <: PermutationAction[G] with Singleton]
  type GC[A <: PermutationAction[G] with Singleton] = GrpChain[G, A]

  /** Returns a faithful permutation action for the group expressed by the given generators. */
  def faithfulAction(generators: Iterable[G]): PermutationAction[G]

  /** Returns a faithful permutation action for the given group; if the group is already represented
    * by a BSGS chain with a faithful action, returns this action.
    */
  def faithfulAction(grp: Grp[G]): PermutationAction[G] = grp match {
    case gc: GrpChain[G, _] if gc.action.isFaithful => gc.action
    case _ => faithfulAction(grp.generators)
  }

  final class MyKernelBuilder(private[this] val generators: Iterable[G]) extends KernelBuilder[G] {

    protected def makeMutableChain(): Generic[G] = {
      val action = faithfulAction(generators)
      MutableChain.empty[G, action.type](action)
    }
  }

  def kernelBuilder(generators: Iterable[G]): KernelBuilder[G] = new MyKernelBuilder(generators)

  def trivial: GenericGC = {
    val action = faithfulAction(IndexedSeq.empty[G])
    trivial(action)
  }

  def trivial(action: PermutationAction[G]): GC[action.type] = {
    implicit def ia: action.type = action
    new GrpChainExplicit[G, action.type](Term[G, action.type], Opt(IndexedSeq.empty[G]), Term[G, action.type])
  }

  def fromGenerators(generators: Seq[G], action: PermutationAction[G], baseGuideOpt: Opt[BaseGuide]): GC[action.type] = {
    implicit def ia: action.type = action
    val kb = kernelBuilder(generators)
    val chain = BuildChain[G, action.type](generators, kb, baseGuideOpt)
    new GrpChainExplicit[G, action.type](chain, Opt(generators), kb.toChain())
  }

  def fromGenerators(generators: Seq[G], action: PermutationAction[G]): GC[action.type] =
    fromGenerators(generators, action, Opt.empty[BaseGuide])

  def fromGenerators(generators: Seq[G]): GenericGC = {
    val action = faithfulAction(generators)
    fromGenerators(generators, action)
  }

  // fromGeneratorsAndOrder

  def fromGeneratorsAndOrder(generators: Seq[G], order: SafeLong,
                             action: PermutationAction[G], baseGuideOpt: Opt[BaseGuide]): GC[action.type] = {
    implicit def ia: action.type = action
    val kb = kernelBuilder(generators)
    val chain = BuildChain[G, action.type](generators, order, kb, baseGuideOpt)
    new GrpChainExplicit[G, action.type](chain, Opt(generators), kb.toChain())
  }

  def fromGeneratorsAndOrder(generators: Seq[G], order: SafeLong, action: PermutationAction[G]): GC[action.type] =
    fromGeneratorsAndOrder(generators, order, action, Opt.empty[BaseGuide])

  def fromGeneratorsAndOrder(generators: Seq[G], order: SafeLong): GenericGC = {
    val action = faithfulAction(generators)
    fromGeneratorsAndOrder(generators, order, action)
  }

  // fromGrp

  def fromGrp(grp: Grp[G]): GenericGC = grp match {
    case gc: GrpChain[G, _] => grp.asInstanceOf[GenericGC]
    case _ => fromGeneratorsAndOrder(grp.generators, grp.order)
  }

  def fromGrp(grp: Grp[G], action: PermutationAction[G]): GC[action.type] = {
    extractGrpChain(grp, action) match {
      case Opt(gc) => gc
      case _ => fromGeneratorsAndOrder(grp.generators, grp.order, action)
    }
  }

  def fromGrp(grp: Grp[G], action: PermutationAction[G], baseGuideOpt: Opt[BaseGuide]): GC[action.type] = baseGuideOpt match {
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
  protected def fromGrpBaseHint(grp: Grp[G], action: PermutationAction[G], baseGuideHintOpt: Opt[BaseGuide]): GC[action.type] =
    extractGrpChain(grp, action) match {
      case Opt(gc) => gc
      case _ => fromGeneratorsAndOrder(grp.generators, grp.order, action, baseGuideHintOpt)
    }

  override def conjugatedBy(grp: Grp[G], h: G): GenericGC = {
      val action: PermutationAction[G] = faithfulAction(grp.generators :+ h)
      implicit val ia: action.type = action
      extractGrpChain(grp, action) match {
        case Opt(conj: GrpChainConjugated[G, action.type]) =>
          assert(conj.kernel.isTrivial) // action is faithful
          new GrpChainConjugated[G, action.type](conj.originalChain, conj.g |+| h, h.inverse |+| conj.gInv, conj.originalGeneratorsOpt, conj.kernel)
        case Opt(cg: GrpChain[G, action.type]) =>
          assert(cg.kernel.isTrivial)
          new GrpChainConjugated[G, action.type](cg.chain, h, h.inverse, Opt(cg.generators), cg.kernel)
        case _ =>
          val hInv = h.inverse
          fromGeneratorsAndOrder(grp.generators.map(g => hInv |+| g |+| h), grp.order)
      }
    }


  /** Computes the union of two groups, deriving a new action in the process.
    *
    * @param lhs           Chain describing the first group under a faithful action. Must be nontrivial.
    * @param rhsGenerators Generators for the second group.
    * @tparam A            Faithful action used to describe the first group, not necessarily valid for rhsGnerators.
    * @return the chain describing the union.
    */
  def faithfulUnionRecomputeAction[A <: PermutationAction[G] with Singleton](lhs: Node[G, A], rhsGenerators: Iterable[G]): Chain.Generic[G] = {
    val newAction: PermutationAction[G] = faithfulAction(lhs.strongGeneratingSet ++ rhsGenerators)
    implicit def ina: newAction.type = newAction
    val newLhs = BuildChain.fromChain[G, A, newAction.type](lhs, Term[G, newAction.type], KernelBuilder.trivial[G])
    newLhs match {
      case lnode1: Node[G, newAction.type] => GrpChain.unionFaithful(lnode1, rhsGenerators)
      case _ => sys.error("Cannot happen, lhs is not the trivial group")
    }
  }

  /** Computes the union of two groups given by BSGS chains, each described by a possibly different faithful action. */
    def faithfulUnion(lhs: Chain.Generic[G], rhs: Chain.Generic[G]): Chain.Generic[G] =
      if (rhs.order > lhs.order) faithfulUnion(rhs, lhs) // ensure that lhs.order >= rhs.order
      else if (rhs.strongGeneratingSet.forall(lhs.siftsFaithful)) lhs
      else lhs match {
        case lnode: Node.Generic[G] => Chain.commonAction(lnode, rhs) match {
          case Opt(action) => GrpChain.unionFaithful(Node.inActionUnsafe(lnode, action), rhs.strongGeneratingSet)
          case _ =>
            val action = lnode.action
            val lnode1 = Node.inActionUnsafe(lnode, action)
            faithfulUnionRecomputeAction(lnode1, rhs.strongGeneratingSet)
        }
        case lterm: Term.Generic[G] => sys.error("Cannot happen, lhs cannot be the trivial group, then detected earlier")
      }

    def union(lhs: Grp[G], rhs: Grp[G]): GenericGC =
      if (rhs.order > lhs.order) union(rhs, lhs) // ensure that lhs.order >= rhs.order
      else if (lhs.hasSubgroup(rhs)) fromGrp(lhs)
      else commonAction(lhs, rhs) match {
        case Opt(action) =>
          val lhs1 = inActionUnsafe(lhs, action)
          val rhs1 = inActionUnsafe(rhs, action)
          val newKernel = faithfulUnion(lhs1.kernel, rhs1.kernel)
          val lhsNewKernel = lhs1.enlargeKernel(newKernel)
          GrpChain.unionGivenKernel(lhsNewKernel, rhs.generators)
        case _ =>
          val action = faithfulAction(lhs.generators ++ rhs.generators)
          GrpChain.unionComputeKernel(fromGrp(lhs, action), rhs.generators, KernelBuilder.trivial[G])
      }

  /** Checks whether the group described by the chain lhsis a subgroup of the group described by the chain rhsGenerators,
    * where both chains are equipped with a faithful action.
    */
  protected def isFaithfulChainSubsetOf(lhs: Chain.Generic[G], rhs: Chain.Generic[G]): Boolean =
    lhs.strongGeneratingSet.forall(g => rhs.sift(g) match {
      case Opt(remainder) => remainder.isId
      case _ => false
    })

  protected def areKernelsEqual(lhs: Chain.Generic[G], rhs: Chain.Generic[G]): Boolean =
    (lhs.order === rhs.order) && isFaithfulChainSubsetOf(lhs, rhs)

  def intersect(lhs: Grp[G], rhs: Grp[G]): GenericGC =
    if (rhs.order > lhs.order) intersect(rhs, lhs) // ensure lhs.order >= rhsGenerators.order
    else if (lhs.hasSubgroup(rhs)) fromGrp(rhs)
    else commonAction(lhs, rhs) match {
      case Opt(action) if areKernelsEqual(inActionUnsafe(lhs, action).kernel, inActionUnsafe(rhs, action).kernel) =>
        val lhs1 = inActionUnsafe(lhs, action)
        val rhs1 = inActionUnsafe(rhs, action)
        GrpChain.intersect[G, action.type](lhs1, rhs1.chain)
      case _ =>
        val action = faithfulAction(lhs.generators ++ rhs.generators)
        val lhs1 = fromGrp(lhs, action)
        val rhs1 = fromGrp(rhs, action)
        val commonKernel = lhs1.kernel
        assert(commonKernel.isTrivial)
        GrpChain.intersect[G, action.type](lhs1, rhs1.chain)
    }

  // GrpStructure

  implicit def grpGroup: GrpGroup[G] = this

  override def smallGeneratingSet(grp: Grp[G]): Seq[G] =
    if (grp.isTrivial) IndexedSeq.empty[G] else {
      val grpChain = fromGrp(grp)
      val fAction = if (grpChain.kernel.isTrivial)
        grpChain.action // the current action is faithful for this particular group
      else
        faithfulAction(grp.generators)
      // if not, create a faithful action
      // TODO: solvable group methods
      val filteredGenerators = FilterOrders(grp.generators, fAction)
      if (filteredGenerators.length <= 1) filteredGenerators else {
        import spire.math.prime.factor
        val abelianizationOrder = grp.order / derivedSubgroup(grp).order
        val min = factor(abelianizationOrder).elements.values.foldLeft(1)(spire.math.max)
        val smallerOpt = fromGrp(grpChain, fAction).chain match {
          case node: Node[G, fAction.type] => schreierSims.reduceGenerators(node, grp.generators, min)
          case term: Term[G, fAction.type] => sys.error("Group has been verified nontrivial above")
        }
        smallerOpt.getOrElseFast(grp.generators)
      }
    }

  def areConjugate(grp: Grp[G], g1: G, g2: G) = findConjugation(grp, g1, g2).nonEmpty

  def areConjugate(grp: Grp[G], g1: G, g2: G, g2CentralizerSubgroup: Opt[Grp[G]]) =
    findConjugation(grp, g1, g2, g2CentralizerSubgroup).nonEmpty

  def findConjugation(grp: Grp[G], g1: G, g2: G): Opt[G] =
    findConjugation(grp, g1, g2, Opt.empty[Grp[G]])

  def findConjugation(grp: Grp[G], g1: G, g2: G, g2CentralizerSubgroup: Opt[Grp[G]]): Opt[G] = {
    if (g2.isId) {
      if (g1.isId) return Opt(grp.group.id) else return Opt.empty[G]
    }
    if (g1.isId) return Opt.empty[G] // we know g2 is not identity
    val action = faithfulAction(grp)
    val sub = g2CentralizerSubgroup match {
      case Opt(s) => fromGrp(s, action)
      case _ => fromGenerators(Seq(g2), action)
    }
    Conjugation.findConjugation(fromGrp(grp, action), g1, g2, sub)
  }

  def centralizer(grp: Grp[G], g: G): Grp[G] = {
    val action = faithfulAction(grp)
    val definition = Conjugation.Centralizer[G, action.type](g)(implicitly, implicitly, action)
    subgroupFor[action.type](grp, action, definition)
  }

  // GrpPermutationAction

  def findSameAction[Q:PermutationAction](grp: Grp[G], action: PermutationAction[G], q: Q) =
    ChainRec.findSameAction[G, action.type, Q](fromGrp(grp, action).chain, q, group.id)(grp.equ, grp.group, implicitly, action)

  def subgroupFor[A <: PermutationAction[G] with Singleton](grp: Grp[G], action: A, definition: SubgroupDefinition[G, A]): GC[A] = {
    implicit def ia: A = action
    val (guidedChain, kernel) = extractGrpChain(grp, action: A) match {
      case Opt(grpChain) => grpChain match {
        case lhs: GrpChainConjugated[G, A] =>
          import lhs.{g, gInv, originalChain}
          val mut = originalChain.mutableChain
          mut.conjugate(g, gInv)
          definition.baseGuideOpt match {
            case Opt(baseGuide) => baseChange.changeBase(mut, lhs.kernel, baseGuide)
            case _ =>
          }
          (mut.toChain(), lhs.kernel)
        case lhs: GrpChain[G, A] =>
          val kb = KernelBuilder.fromChain(lhs.kernel)
          (BuildChain.fromChain[G, A, A](lhs.chain, lhs.kernel, kb, definition.baseGuideOpt), lhs.kernel)
      }
      case _ =>
        val kb = kernelBuilder(grp.generators)
        val chain = BuildChain[G, A](grp.generators, grp.order, kb, definition.baseGuideOpt)
        (chain, kb.toChain())
    }
    val subChain = SubgroupSearch.subgroupSearch(definition, guidedChain, kernel).toChain()
    new GrpChainExplicit[G, A](subChain, Opt.empty[Seq[G]], kernel)
  }

  def someStabilizerTransversal(grp: Grp[G], action: PermutationAction[G]): Opt[(GrpChain[G, action.type], Transversal[G, action.type])] =
    GrpChain.someStabilizerTransversal[G, action.type](fromGrp(grp, action))

  def stabilizer(grp: Grp[G], action: PermutationAction[G], p: Int): GrpChain[G, action.type] =
    GrpChain.stabilizer[G, action.type](fromGrpBaseHint(grp, action, Opt(BaseGuideSingle(p))), p)

  def stabilizerTransversal(grp: Grp[G], action: PermutationAction[G], p: Int): (GC[action.type], Transversal[G, action.type]) =
    GrpChain.stabilizerTransversal[G, action.type](fromGrpBaseHint(grp, action, Opt(BaseGuideSingle(p))), p)

  def pointwiseStabilizer(grp: Grp[G], action: PermutationAction[G], set: Set[Int]): GC[action.type] =
    GrpChain.pointwiseStabilizer[G, action.type](fromGrpBaseHint(grp, action, Opt(BaseGuideSet(set))), set)

  def orderedPartitionStabilizer(grp: Grp[G], action: PermutationAction[G], partition: Partition): GC[action.type] =
    subgroupFor[action.type](grp, action, net.alasc.bsgs.OrderedPartitionStabilizer[G, action.type](partition)(implicitly, action))

  def unorderedPartitionStabilizer(grp: Grp[G], action: PermutationAction[G], partition: Partition): GC[action.type] =
    subgroupFor[action.type](grp, action, net.alasc.bsgs.UnorderedPartitionStabilizer[G, action.type](partition)(implicitly, action))

  def setwiseStabilizer(grp: Grp[G], action: PermutationAction[G], set: Set[Int]): GC[action.type] =
    subgroupFor[action.type](grp, action, SetwiseStabilizer[G, action.type](set)(implicitly, action))

  def subgroupFor(grp: Grp[G], action: PermutationAction[G], backtrackTest: (Int, Int) => Boolean, predicate: Perm => Boolean): GC[action.type] =
    subgroupFor[action.type](grp, action, SubgroupDefinition[G, action.type](backtrackTest, g => predicate(action.toPerm(g)))(action))

  def leftCosetsBy(grp: Grp[G], subgrp: Grp[G]): LeftCosets[G, subgrp.type] =
    GrpChain.extractAction(grp) match {
      case Opt(action) if inActionUnsafe(grp, action).kernel.isTrivial =>
        GrpChain.leftCosetsByFaithfulAction(inActionUnsafe(grp, action), subgrp, fromGrp(subgrp, action))
      case _ =>
        val newAction = faithfulAction(grp.generators)
        GrpChain.leftCosetsByFaithfulAction(fromGrp(grp, newAction), subgrp, fromGrp(subgrp, newAction))
    }

  def rightCosetsBy(grp: Grp[G], subgrp: Grp[G]): RightCosets[G, subgrp.type] = leftCosetsBy(grp, subgrp).inverse

  def lexElements(grp: Grp[G], action: PermutationAction[G]): Opt[BigIndexedSeq[G]] = {
    val cg = fromGrp(grp, action)
    if (cg.kernel.isTrivial) { // action is faithful
      val n = action.largestMovedPoint(grp.generators).getOrElseFast(0) + 1
      Opt(new GrpChain.LexElements[G, action.type](fromGrp(cg, action, Opt(BaseGuideLex(n)))))
    } else Opt.empty[BigIndexedSeq[G]]
  }

  def base(grp: Grp[G], action: PermutationAction[G]): Opt[Seq[Int]] = {
    val cg = fromGrp(grp, action)
    if (cg.kernel.isTrivial) { // action is faithful
      cg match {
        case cj: GrpChainConjugated[G, action.type] => Opt(cj.originalChain.base.map(action.actr(_, cj.g)))
        case cg => Opt(cg.chain.base)
      }
    } else Opt.empty[Seq[Int]]
  }

  def toPerm(grp: Grp[G], action: PermutationAction[G])(implicit builder: GrpGroup[Perm]): Grp[Perm] = {
    val gc = fromGrp(grp, action)
    builder.fromGeneratorsAndOrder(gc.generators.map(action.toPerm), grp.order / gc.kernel.order)
  }

  def kernel(grp: Grp[G], action: PermutationAction[G]): Grp[G] =
    fromGrp(grp, action).kernel match {
      case node: Node[G, _] =>
        val action = node.action
        implicit def ia: action.type = action
        val nodeInAction = Node.inActionUnsafe(node, action)
        new GrpChainExplicit(nodeInAction, Opt.empty[Seq[G]], Term[G, action.type])
      case _ =>
        val action = faithfulAction(Seq.empty[G])
        implicit def ia: action.type = action
        new GrpChainExplicit(Term[G, action.type], Opt(Seq.empty[G]), Term[G, action.type])
    }

}

object GrpChainPermutationAction {

  @inline final def apply[G](implicit ev: GrpChainPermutationAction[G]): GrpChainPermutationAction[G] = ev

}
