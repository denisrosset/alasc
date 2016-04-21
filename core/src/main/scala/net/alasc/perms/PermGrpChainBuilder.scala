package net.alasc.perms

import scala.reflect.ClassTag

import spire.math.SafeLong
import spire.syntax.action._
import spire.syntax.group._
import spire.util.Opt

import net.alasc.algebra.{BigIndexedSeq, Permutation, PermutationAction}
import net.alasc.bsgs._
import net.alasc.domains.Partition
import net.alasc.finite.{Grp, LeftCosets, RightCosets}

class PermGrpChainBuilder[G, F <: Permutation[G] with Singleton](implicit
                             val baseChange: BaseChange,
                             val baseSwap: BaseSwap,
                             val classTag: ClassTag[G],
                             val permutation: F,
                             val schreierSims: SchreierSims
                            ) extends PermGrpBuilder[G] {

  type GG = GrpChain[G, F]

  // builder methods

  def trivial = new GrpChainExplicit[G, F](Term[G, F], generatorsOpt = Opt(Iterable.empty[G]))

  def fromGenerators(generators: Iterable[G]): GG =
    fromGenerators(generators, Opt.empty[BaseGuide])

  def fromGenerators(generators: Iterable[G], baseGuideOpt: Opt[BaseGuide]): GG = {
    val chain = BuildChain.fromGenerators[G, F](generators, baseGuideOpt)
    new GrpChainExplicit[G, F](chain, generatorsOpt = Opt(generators))
  }

  def fromGeneratorsAndOrder(generators: Iterable[G], order: SafeLong): GG =
    fromGeneratorsAndOrder(generators, order, Opt.empty[BaseGuide])

  def fromGeneratorsAndOrder(generators: Iterable[G], order: SafeLong, baseGuideOpt: Opt[BaseGuide]): GG = {
    val chain = BuildChain.fromGeneratorsAndOrder[G, F](generators, order, baseGuideOpt)
    new GrpChainExplicit(chain, generatorsOpt = Opt(generators))
  }

  def fromGrp(grp: Grp[G]): GG = grp match {
    case gc: GrpChain[G, _] if gc.action eq permutation => gc.asInstanceOf[GrpChain[G, F]]
    case _ => fromGeneratorsAndOrder(grp.generators, grp.order)
  }

  def fromGrp(grp: Grp[G], baseGuideOpt: Opt[BaseGuide]): GG = baseGuideOpt match {
    case Opt(baseGuide) => grp match {
      case gc: GrpChain[G, _] if gc.action eq permutation => gc.asInstanceOf[GrpChain[G, F]].chainOpt match {
        case Opt(chain) if baseGuide.isSatisfiedBy(chain) => gc.asInstanceOf[GrpChain[G, F]]
        case _ => fromGeneratorsAndOrder(grp.generators, grp.order, baseGuideOpt)
      }
      case _ => fromGeneratorsAndOrder(grp.generators, grp.order, baseGuideOpt)
    }
    case _ => fromGrp(grp)
  }

  /** Retrieves a `GrpChain` from `grp`, uses the base guide if the chain needs to be computed from scratch. */
  protected def convertGrp(grp: Grp[G], baseGuide: BaseGuide) = grp match {
    case gc: GrpChain[G, _] if gc.action eq permutation => gc.asInstanceOf[GrpChain[G, F]]
    case _ => fromGeneratorsAndOrder(grp.generators, grp.order, Opt(baseGuide))
  }

  /** Retrieves a `GrpChain` from `grp`. */
  protected def convertGrp(grp: Grp[G]) = grp match {
    case gc: GrpChain[G, _] if gc.action eq permutation => gc.asInstanceOf[GrpChain[G, F]]
    case _ => fromGeneratorsAndOrder(grp.generators, grp.order)
  }

  // with a single Grp[G] argument

  def someStabilizerTransversal(grp: Grp[G]): Opt[(GG, Transversal[G, F])] =
    GrpChain.someStabilizerTransversal[G, F](convertGrp(grp))

  def stabilizer(grp: Grp[G], b: Int): GG =
    GrpChain.stabilizer[G, F](convertGrp(grp, BaseGuideSeq(Seq(b))), b)

  def stabilizerTransversal(grp: Grp[G], b: Int): (GG, Transversal[G, F]) =
    GrpChain.stabilizerTransversal[G, F](convertGrp(grp, BaseGuideSeq(Seq(b))), b)

  def pointwiseStabilizer(grp: Grp[G], set: Set[Int]): GG =
    GrpChain.pointwiseStabilizer[G, F](convertGrp(grp, BaseGuideSet(set)), set)

  override def conjugatedBy(grp: Grp[G], h: G): GG = grp match {
    case conj1: GrpChainConjugated[G, _] if conj1.action eq permutation =>
      val conj = conj1.asInstanceOf[GrpChainConjugated[G, F]]
      new GrpChainConjugated[G, F](conj.originalChain, conj.g |+| h, h.inverse |+| conj.gInv,
        originalGeneratorsOpt = Opt(conj.originalGenerators))
    case cg1: GrpChain[G, _] if cg1.action eq permutation =>
      val cg = cg1.asInstanceOf[GrpChain[G, F]]
      new GrpChainConjugated[G, F](cg.chain, h, h.inverse,
        originalGeneratorsOpt = Opt(cg.generators))
    case _ =>
      val hInv = h.inverse
      fromGeneratorsAndOrder(grp.generators.map(g => hInv |+| g |+| h), grp.order)
  }

  def intersect(lhs: Grp[G], rhs: Grp[G]): GG =
    if (rhs.order > lhs.order) intersect(rhs, lhs) // ensure lhs.order >= rhs.order
    else if (lhs.hasSubgroup(rhs)) fromGrp(rhs)
    else {
      val rhsChain = convertGrp(rhs).chain
      subgroupFor(lhs, Intersection[G, F](rhsChain))
    }

  def subgroupFor(grp: Grp[G], definition: SubgroupDefinition[G, F]): GG = {
    val guidedChain = grp match {
      case lhs1: GrpChainConjugated[G, _] if lhs1.action eq permutation =>
        val lhs = lhs1.asInstanceOf[GrpChainConjugated[G, F]]
        import lhs.{g, gInv, originalChain}
        val mut = originalChain.mutableChain
        mut.conjugate(g, gInv)
        definition.baseGuideOpt match {
          case Opt(baseGuide) => baseChange.changeBase(mut, baseGuide)
          case _ =>
        }
        mut.toChain()
      case lhs: GrpChain[G, _] if lhs.action eq permutation =>
        BuildChain.fromChain[G, F, F](lhs.asInstanceOf[GrpChain[G, F]].chain, definition.baseGuideOpt)
      case _ =>
        BuildChain.fromGeneratorsAndOrder[G, F](grp.generators, grp.order, definition.baseGuideOpt)
    }
    val subChain = SubgroupSearch.subgroupSearch(definition, guidedChain).toChain()
    new GrpChainExplicit[G, F](subChain)
  }

  def union(lhs: Grp[G], rhs: Grp[G]): GG =
    if (rhs.order > lhs.order) union(rhs, lhs) // ensure that lhs.order is the greatest
    else if (lhs.hasSubgroup(rhs)) fromGrp(lhs)
    else {
      val lhsChain = convertGrp(lhs).chain
      val mutableChain = BuildMutableChain.fromChain[G, F, F](lhsChain)
      val newGenerators = rhs.generators.filterNot(mutableChain.start.next.sifts)
      mutableChain.insertGenerators(newGenerators)
      mutableChain.completeStrongGenerators()
      val newChain = mutableChain.toChain()
      val generatorsOpt =
        if (newChain.strongGeneratingSet.size >= lhs.generators.size + newGenerators.size)
          Opt(lhs.generators ++ newGenerators)
        else
          Opt.empty[Iterable[G]]
      new GrpChainExplicit(mutableChain.toChain(), generatorsOpt)
    }

  def subgroupFor(grp: Grp[G], backtrackTest: (Int, Int) => Boolean, predicate: G => Boolean): GG =
    subgroupFor(convertGrp(grp), SubgroupDefinition[G, F](backtrackTest, predicate))

  def fixingPartition(grp: Grp[G], partition: Partition): GG =
    subgroupFor(grp, net.alasc.bsgs.FixingPartition[G, F](partition))

  def setwiseStabilizer(grp: Grp[G], set: Set[Int]): GG =
    subgroupFor(grp, SetwiseStabilizer[G, F](set))

  def leftCosetsBy(grp: Grp[G], subgrp: Grp[G]): LeftCosets[G] =
    GrpChain.leftCosetsBy[G, F](convertGrp(grp), convertGrp(subgrp))

  def rightCosetsBy(grp: Grp[G], subgrp: Grp[G]): RightCosets[G] =
    leftCosetsBy(grp, subgrp).inverse

  // enumeration of subgroup elements
  def lexElements(grp: Grp[G]): BigIndexedSeq[G] = {
    val cg = fromGrp(grp)
    val n = PermutationAction.largestMovedPoint(grp.generators).getOrElseFast(0) + 1
    new GrpChain.LexElements[G, F](convertGrp(grp, BaseGuideLex(n)))
  }

  def base(grp: Grp[G]): Seq[Int] = convertGrp(grp) match {
    case cj: GrpChainConjugated[G, _] => cj.originalChain.base.map(_ <|+| cj.g)
    case cg => cg.chain.base
  }

  def find[Q:Permutation](grp: Grp[G], q: Q): Opt[G] =
    convertGrp(grp).chain.siftOther(q)

}
