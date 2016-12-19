package net.alasc.bsgs

import scala.reflect.ClassTag
import scala.util.Random

import spire.algebra.{Eq, Group}
import spire.math.SafeLong
import spire.syntax.group._
import spire.util.Opt

import net.alasc.algebra.PermutationAction
import net.alasc.finite.Grp

object BuildMutableChain {

  /*
  def fromChainAndKernel[G:ClassTag:Eq:Group, F <: PermutationAction[G] with Singleton]
    (from: Chain[G, _ <: PermutationAction[G] with Singleton], oldKernel: Grp[G], kernel: KernelBuilder[G], baseStart: Seq[Int] = Seq.empty[Int])
    (implicit newAction: F, schreierSims: SchreierSims): MutableChain[G, F] = {
    val generators = from.strongGeneratingSet ++ oldKernel.generators
    def randomElement(rand: Random): G = from.randomElement(rand) |+| oldKernel.randomElement(rand)
    val order = from.order * oldKernel.order
    schreierSims.completeChainFromGeneratorsRandomElementsAndOrder[G, F](generators, randomElement, order, kernel, baseStart)
  }

  def fromChain[G:ClassTag:Eq:Group, F1 <: PermutationAction[G] with Singleton, F2 <: PermutationAction[G] with Singleton]
  (from: Chain[G, F1], oldKernel: Grp[G], kernel: KernelBuilder[G])(implicit newAction: F2, schreierSims: SchreierSims): MutableChain[G, F2] =
    from match {
      case node: Node[G, F1] if node.action eq newAction =>
        kernel.absorb(oldKernel)
        node.asInstanceOf[Node[G, F2]].mutableChain
      case term: Term[G, F1] if oldKernel.generators.forall(!newAction.movesAnyPoint(_)) =>
        // kernel group is the same under the new action
        kernel.absorb(oldKernel)
        MutableChain.empty[G, F2]
      case _ => // different action
        fromChainAndKernel[G, F2](from, oldKernel, kernel)
    }

  def fromChain[G:ClassTag:Eq:Group, F1 <: PermutationAction[G] with Singleton, F2 <: PermutationAction[G] with Singleton]
  (from: Chain[G, F1], oldKernel: Grp[G], kernel: KernelBuilder[G], baseGuideOpt: Opt[BaseGuide])
  (implicit newAction: F2, baseChange: BaseChange, schreierSims: SchreierSims): MutableChain[G, F2] = {
    val baseGuide = baseGuideOpt.getOrElseFast(BaseGuide.Empty)
    val mutableChain = from match {
      case node: Node[G, F1] if node.action eq newAction =>
        kernel.absorb(oldKernel)
        node.asInstanceOf[Node[G, F2]].mutableChain
      case term: Term[G, F1] if oldKernel.generators.forall(!newAction.movesAnyPoint(_)) =>
        // kernel group is the same under the new action
        kernel.absorb(oldKernel)
        MutableChain.empty[G, F2]
      case _ => // different action
        val baseStart = baseGuide.baseAnsatz[G, F2](from.strongGeneratingSet ++ oldKernel.generators)
        fromChainAndKernel[G, F2](from, oldKernel, kernel, baseStart)
    }
    baseChange.changeBase(mutableChain, baseGuide)
    mutableChain
  }

  def fromGenerators[G:ClassTag:Eq:Group, F <: PermutationAction[G] with Singleton]
  (generators: Iterable[G], kernel: KernelBuilder[G])(implicit action: F, schreierSims: SchreierSims): MutableChain[G, F] =
    schreierSims.completeChainFromGenerators[G, F](generators, kernel)

  def fromGenerators[G:ClassTag:Eq:Group, F <: PermutationAction[G] with Singleton]
  (generators: Iterable[G], kernel: KernelBuilder[G], baseGuideOpt: Opt[BaseGuide])
  (implicit action: F, baseChange: BaseChange, schreierSims: SchreierSims): MutableChain[G, F] = {
    val baseGuide = baseGuideOpt.getOrElseFast(BaseGuide.Empty)
    val ansatz = baseGuide.baseAnsatz[G, F](generators)
    val mutableChain = schreierSims.completeChainFromGenerators[G, F](generators, kernel, ansatz)
    baseChange.changeBase(mutableChain, baseGuide)
    mutableChain
  }

  def fromGeneratorsAndOrder[G:ClassTag:Eq:Group, F <: PermutationAction[G] with Singleton]
  (generators: Iterable[G], order: SafeLong, kernel: KernelBuilder[G])(implicit action: F, schreierSims: SchreierSims): MutableChain[G, F] =
    schreierSims.completeChainFromGeneratorsAndOrder[G, F](generators, order, kernel)

  def fromGeneratorsAndOrder[G:ClassTag:Eq:Group, F <: PermutationAction[G] with Singleton]
    (generators: Iterable[G], order: SafeLong, kernel: KernelBuilder[G], baseGuideOpt: Opt[BaseGuide] = Opt.empty[BaseGuide])
    (implicit action: F, baseChange: BaseChange, schreierSims: SchreierSims): MutableChain[G, F] = {
    val baseGuide = baseGuideOpt.getOrElseFast(BaseGuide.Empty)
    val ansatz = baseGuide.baseAnsatz[G, F](generators)
    val mutableChain = schreierSims.completeChainFromGeneratorsAndOrder[G, F](generators, order, kernel, ansatz)
    baseChange.changeBase(mutableChain, baseGuide)
    mutableChain
  }

  def fromGeneratorsRandomElementsAndOrder[G:ClassTag:Eq:Group, F <: PermutationAction[G] with Singleton]
  (generators: Iterable[G], randomElement: Random => G, order: SafeLong, kernel: KernelBuilder[G])(implicit action: F, schreierSims: SchreierSims): MutableChain[G, F] =
    schreierSims.completeChainFromGeneratorsRandomElementsAndOrder[G, F](generators, randomElement, order, kernel)


  def fromGeneratorsRandomElementsAndOrder[G:ClassTag:Eq:Group, F <: PermutationAction[G] with Singleton]
    (generators: Iterable[G], randomElement: Random => G, order: SafeLong, kernel: KernelBuilder[G], baseGuideOpt: Opt[BaseGuide] = Opt.empty[BaseGuide])
    (implicit action: F, baseChange: BaseChange, schreierSims: SchreierSims): MutableChain[G, F] = {
    val baseGuide = baseGuideOpt.getOrElseFast(BaseGuide.Empty)
    val ansatz = baseGuide.baseAnsatz[G, F](generators)
    val mutableChain = schreierSims.completeChainFromGeneratorsRandomElementsAndOrder[G, F](generators, randomElement, order, kernel, ansatz)
    baseChange.changeBase(mutableChain, baseGuide)
    mutableChain
  }*/

}
