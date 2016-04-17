package net.alasc.prep
package chain

import scala.reflect.ClassTag
import scala.util.Random

import spire.algebra.{Order, Eq, Group}
import spire.syntax.group._
import spire.util.Opt

import net.alasc.finite._

import bsgs._

class PGrpChainBuilder[G](implicit
    val baseChange: BaseChange,
    val baseSwap: BaseSwap,
    val classTag: ClassTag[G],
    val defaultRepBuilder: PRepBuilder[G],
    val equ: Eq[G],
    val group: Group[G],
    val schreierSims: SchreierSims
) extends PGrpBuilder[G] {

  type GG = PGrpChain[G]

  implicit def builder: PGrpChainBuilder[G] = this

  // bridge methods adding an empty baseguide

  def fromGeneratorsIn(pRep: FaithfulPRep[G])
                      (generators: Iterable[G]): PGrpChain.In[pRep.type, G] =
    fromGeneratorsIn(pRep, Opt.empty[BaseGuide])(generators)

  def fromGeneratorsAndOrderIn(pRep: FaithfulPRep[G])
                              (generators: Iterable[G], order: BigInt): PGrpChain.In[pRep.type, G] =
    fromGeneratorsAndOrderIn(pRep, Opt.empty[BaseGuide])(generators, order)

  def fromGeneratorsRandomElementsAndOrderIn(pRep: FaithfulPRep[G])
                                            (generators: Iterable[G], randomElement: Random => G, order: BigInt): PGrpChain.In[pRep.type, G] =
    fromGeneratorsRandomElementsAndOrderIn(pRep, Opt.empty[BaseGuide])(generators, randomElement, order)

  def fromGrpIn(pRep: FaithfulPRep[G])(grp: Grp[G]): PGrpChain.In[pRep.type, G] =
    fromGrpIn(pRep, Opt.empty[BaseGuide])(grp)

  def fromChainIn(pRep: FaithfulPRep[G])(chain: Chain[G]): PGrpChain.In[pRep.type, G] =
    fromChainIn(pRep, Opt.empty[BaseGuide])(chain)

  // methods that retrieve a representation from the default builder (TODO: construct a lazy PGrp)

  def trivial: PGrpChain[G] = {
    val rep = defaultRepBuilder.lattice.zero
    trivialIn(rep)
  }

  def fromGenerators(generators: Iterable[G]): PGrpChain[G] = {
    val rep: FaithfulPRep[G] = defaultRepBuilder.build(generators)
    fromGeneratorsIn(rep)(generators)
  }

  def fromGeneratorsAndOrder(generators: Iterable[G], order: BigInt): PGrpChain[G] = {
    val rep: FaithfulPRep[G] = defaultRepBuilder.build(generators)
    fromGeneratorsAndOrderIn(rep)(generators, order)
  }

  def fromChain(chain: Chain[G]): PGrpChain[G] = {
    val rep: FaithfulPRep[G] = defaultRepBuilder.build(chain.strongGeneratingSet)
    fromChainIn(rep)(chain)
  }

  def fromGrp(grp: Grp[G]): PGrpChain[G] = grp match {
    case pg: PGrpChain[G] => pg
    case _ =>
      fromGeneratorsAndOrder(grp.generators, grp.order)
  }

  // implementations

  def trivialIn(pRep: FaithfulPRep[G]): PGrpChain.In[pRep.type, G] =
    new PGrpExplicit[pRep.type, G](pRep, Term[G])

  def fromGeneratorsIn(pRep: FaithfulPRep[G], baseGuideOpt: Opt[BaseGuide])
                      (generators: Iterable[G]): PGrpChain.In[pRep.type, G] = {
    val chain = BuildChain.fromGenerators(generators, pRep.permutationAction, baseGuideOpt)
    new PGrpExplicit[pRep.type, G](pRep, chain, Opt(generators))
  }

  def fromGeneratorsAndOrderIn(pRep: FaithfulPRep[G], baseGuideOpt: Opt[BaseGuide])
                              (generators: Iterable[G], order: BigInt): PGrpChain.In[pRep.type, G] = {
    val chain = BuildChain.fromGeneratorsAndOrder(
      generators, order, pRep.permutationAction, baseGuideOpt)
    new PGrpExplicit[pRep.type, G](pRep, chain, Opt(generators))
  }

  def fromGeneratorsRandomElementsAndOrderIn(pRep: FaithfulPRep[G], baseGuideOpt: Opt[BaseGuide])
                                            (generators: Iterable[G], randomElement: Random => G, order: BigInt): PGrpChain.In[pRep.type, G] = {
    val chain = BuildChain.fromGeneratorsRandomElementsAndOrder(
      generators, randomElement, order, pRep.permutationAction, baseGuideOpt)
    new PGrpExplicit[pRep.type, G](pRep, chain, Opt(generators))
  }

  def fromChainIn(pRep: FaithfulPRep[G], baseGuideOpt: Opt[BaseGuide])(chain: Chain[G]): PGrpChain.In[pRep.type, G] = {
    val newChain = BuildChain.fromChain(chain, pRep.permutationAction, baseGuideOpt)
    new PGrpExplicit[pRep.type, G](pRep, newChain)
  }

  def fromChainSubgroupOfIn(parent: Grp[G])(pRep: FaithfulPRep[G], baseGuideOpt: Opt[BaseGuide])(chain: Chain[G]): PGrpChain.In[pRep.type, G] = {
    val newChain = BuildChain.fromChain(chain, pRep.permutationAction, baseGuideOpt)
    new PGrpExplicit[pRep.type, G](pRep, newChain, Opt.empty[Iterable[G]])
  }

  def fromGrpIn(pRep: FaithfulPRep[G], baseGuideOpt: Opt[BaseGuide])
               (grp: Grp[G]): PGrpChain.In[pRep.type, G] = grp match {
    case pg: PGrpConjugated[_, G] if pg.pRep.permutationAction == pRep.permutationAction =>
      val mut = imply(pRep.permutationAction) {
        pg.originalChain.mutableChain
      }
      mut.conjugate(pg.g, pg.gInv)
      baseGuideOpt match {
        case Opt(baseGuide) => baseChange.changeBase(mut, baseGuide)
        case _ =>
      }
      fromChainSubgroupOfIn(grp)(pRep, Opt.empty[BaseGuide])(mut.toChain())
    case pg: PGrpChain[G] if (pg.pRep eq pRep) && baseGuideOpt.isEmpty =>
      pg.asInstanceOf[PGrpChain.In[pRep.type, G]]
    case pg: PGrpChain[G] if pg.chainOpt.nonEmpty =>
      fromChainSubgroupOfIn(grp)(pRep, baseGuideOpt)(pg.chain)
    case _ =>
      // TODO: optimize this allocation
      fromGeneratorsRandomElementsAndOrderIn(pRep)(grp.generators, grp.randomElement, grp.order)
  }

  def unionByAdding(pRep: FaithfulPRep[G], chain: Chain[G], generatorsToAdd: Iterable[G]): GG = {
    val mutableChain = BuildMutableChain.fromChain(chain, pRep.permutationAction)
    mutableChain.insertGenerators(generatorsToAdd)
    mutableChain.completeStrongGenerators()
    val newChain: Chain[G] = mutableChain.toChain()
    new PGrpExplicit[pRep.type, G](pRep, newChain)
  }

  def union(lhs: Grp[G], rhs: Grp[G]): GG =
    if (rhs.order > lhs.order) union(rhs, lhs) // ensure lhs.order >= rhs.order
    else if (lhs.hasSubgroup(rhs)) fromGrp(lhs)
    else lhs match {
      case pLhs: PGrpChain[G] if rhs.generators.forall(pLhs.pRep.represents) =>
        unionByAdding(pLhs.pRep, pLhs.chain, rhs.generators)
      case _ =>
        val newRep = builder.defaultRepBuilder.build(lhs.generators ++ rhs.generators)
        unionByAdding(newRep, fromGrpIn(newRep)(lhs).chain, rhs.generators)
    }

  def intersect(lhs: Grp[G], rhs: Grp[G]): GG =
    if (rhs.order > lhs.order) intersect(rhs, lhs) // ensure lhs.order >= rhs.order
    else if (lhs.hasSubgroup(rhs)) fromGrp(rhs)
    else (lhs, rhs) match {
      case (pLhs: PGrpChain[G], pRhs: PGrp[G]) if rhs.generators.forall(pLhs.pRep.represents) =>
        val newRhsChain = fromGrpIn(pLhs.pRep)(rhs).chain
        subgroupFor(pLhs, Intersection(pLhs.pRep.permutationAction, newRhsChain))
      case _ =>
        val newRep = defaultRepBuilder.build(lhs.generators ++ rhs.generators)
        val newLhsGrp = fromGrpIn(newRep)(lhs)
        val newRhsChain = fromGrpIn(newRep)(rhs).chain
        subgroupFor(newLhsGrp, Intersection(newRep.permutationAction, newRhsChain))
    }

  def subgroupFor(grp: PGrp[G], backtrackTest: (Int, Int) => Boolean, predicate: G => Boolean): GG = grp match {
    case cGrp: PGrpChain[G] =>
      subgroupFor(cGrp, SubgroupDefinition(grp.pRep.permutationAction, backtrackTest, predicate))
    case _ => subgroupFor(fromGrpIn(grp.pRep)(grp): PGrpChain[G], backtrackTest, predicate)
  }

  def subgroupFor(grp: PGrpChain[G], definition: SubgroupDefinition[G]): GG = grp match {
    case lhs: PGrpConjugated[_, G] =>
      import lhs.{pRep, originalChain, g, gInv}
      val mut = imply(pRep.permutationAction) { originalChain.mutableChain }
      mut.conjugate(g, gInv)
      definition.baseGuideOpt match {
        case Opt(baseGuide) => baseChange.changeBase(mut, baseGuide)
        case _ =>
      }
      val guidedChain = mut.toChain()
      val result = SubgroupSearch.subgroupSearch(definition, guidedChain).toChain()
      new PGrpExplicit[pRep.type, G](pRep, result, Opt.empty[Iterable[G]])
    case lhs: PGrpChain[G] =>
      import lhs.{chain, pRep}
      val guidedChain = BuildChain.fromChain(chain, pRep.permutationAction, definition.baseGuideOpt)
      val result = SubgroupSearch.subgroupSearch(definition, guidedChain).toChain()
      new PGrpExplicit[pRep.type, G](pRep, result, Opt.empty[Iterable[G]])
  }

  def leftCosetsBy(grp0: Grp[G], subgrp0: Grp[G]): LeftCosets[G] =
    new LeftCosets[G] {

      val grp: PGrpChain[G] = grp0 match {
        case cGrp: PGrpChain[G] => cGrp
        case _ => fromGrp(grp0)
      }
      val subgrp: Grp[G] = subgrp0

      def iterator: Iterator[LeftCoset[G]] = {
        val bo = BaseOrder(grp.pRep.permutationAction, grp.chain.base)
        def rec(g: G, chain: Chain[G], subSubgrp: Grp[G]): Iterator[LeftCoset[G]] = chain match {
          case node: Node[G] =>
            for {
              b <- node.orbit.iterator
              bg = grp.pRep.permutationAction.actr(b, g)
              (nextSubSubGrp, transversal) = subSubgrp.in(grp.pRep).stabilizerTransversal(bg) if transversal.orbit.min(Order.ordering(bo)) == bg
              nextG = node.u(b) |+| g
              element <- rec(nextG, node.next, nextSubSubGrp)
            } yield element
          case _: Term[G] =>
            assert(subSubgrp.order == 1)
            Iterator(new LeftCoset(g, subgrp))
        }
        rec(Group[G].id, grp.chain, subgrp)
      }

    }

  def rightCosetsBy(grp0: Grp[G], subgrp0: Grp[G]): RightCosets[G] =
    new RightCosets[G] {
      val grp = grp0
      val subgrp = subgrp0
      def iterator = leftCosetsBy(grp0, subgrp0).iterator.map(_.inverse)
    }

}
