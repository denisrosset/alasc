package net.alasc.prep
package chain

import scala.reflect.ClassTag
import scala.util.Random

import spire.algebra.{Eq, Group, Order}
import spire.syntax.group._
import spire.util.Opt

import net.alasc.algebra.Permutation
import net.alasc.domains.Partition
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

  def fromGeneratorsRandomElementsAndOrder(generators: Iterable[G], randomElement: Random => G, order: BigInt): PGrpChain[G] = {
    val rep: FaithfulPRep[G] = defaultRepBuilder.build(generators)
    fromGeneratorsRandomElementsAndOrderIn(rep)(generators, randomElement, order)
  }

  def fromChain(chain: Chain[G]): PGrpChain[G] = {
    val rep: FaithfulPRep[G] = defaultRepBuilder.build(chain.strongGeneratingSet)
    fromChainIn(rep)(chain)
  }

  def fromGrp(grp: Grp[G]): PGrpChain[G] = grp match {
    case pg: PGrpChain[G] => pg
    case _ =>
      fromGeneratorsRandomElementsAndOrder(grp.generators, grp.randomElement, grp.order)
  }

  // implementations

  def trivialIn(pRep: FaithfulPRep[G]): PGrpChain.In[pRep.type, G] =
    new PGrpExplicit[pRep.type, G](pRep, Term[G])

  def fromGeneratorsIn(pRep: FaithfulPRep[G], baseGuideOpt: Opt[BaseGuide] = Opt.empty[BaseGuide])
    (generators: Iterable[G]): PGrpChain.In[pRep.type, G] = {
    val chain = BuildChain.fromGenerators(generators, pRep.permutationAction, baseGuideOpt)
    new PGrpExplicit[pRep.type, G](pRep, chain, Opt(generators))
  }

  def fromGeneratorsAndOrderIn(pRep: FaithfulPRep[G], baseGuideOpt: Opt[BaseGuide] = Opt.empty[BaseGuide])
    (generators: Iterable[G], order: BigInt): PGrpChain.In[pRep.type, G] = {
    val chain = BuildChain.fromGeneratorsAndOrder(
      generators, order, pRep.permutationAction, baseGuideOpt)
    new PGrpExplicit[pRep.type, G](pRep, chain, Opt(generators))
  }

  def fromGeneratorsRandomElementsAndOrderIn(pRep: FaithfulPRep[G], baseGuideOpt: Opt[BaseGuide] = Opt.empty[BaseGuide])
    (generators: Iterable[G], randomElement: Random => G, order: BigInt): PGrpChain.In[pRep.type, G] = {
    val chain = BuildChain.fromGeneratorsRandomElementsAndOrder(
      generators, randomElement, order, pRep.permutationAction, baseGuideOpt)
    new PGrpExplicit[pRep.type, G](pRep, chain, Opt(generators))
  }

  def fromChainIn(pRep: FaithfulPRep[G], baseGuideOpt: Opt[BaseGuide] = Opt.empty[BaseGuide])(chain: Chain[G]): PGrpChain.In[pRep.type, G] = {
    val newChain = BuildChain.fromChain(chain, pRep.permutationAction, baseGuideOpt)
    new PGrpExplicit[pRep.type, G](pRep, newChain)
  }

  def fromGrpIn(pRep: FaithfulPRep[G], baseGuideOpt: Opt[BaseGuide])
    (grp: Grp[G]): PGrpChain.In[pRep.type, G] = grp match {
    case pg: PGrpConjugated[_, G] if pg.pRep.permutationAction == pRep.permutationAction =>
      val mut = imply(pRep.permutationAction) { pg.originalChain.mutableChain }
      mut.conjugate(pg.g, pg.gInv)
      baseGuideOpt match {
        case Opt(baseGuide) => baseChange.changeBase(mut, baseGuide)
        case _ =>
      }
      fromChainIn(pRep)(mut.toChain())
    case pg: PGrpChain[G] if (pg.pRep eq pRep) && baseGuideOpt.isEmpty =>
      pg.asInstanceOf[PGrpChain.In[pRep.type, G]]
    case pg: PGrpChain[G] if pg.chainOpt.nonEmpty =>
      fromChainIn(pRep, baseGuideOpt)(pg.chain)
    case _ =>
      fromGeneratorsRandomElementsAndOrderIn(pRep)(grp.generators, grp.randomElement, grp.order)
  }

}
