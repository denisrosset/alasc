package net.alasc.prep.bsgs

import scala.reflect.ClassTag
import scala.util.Random

import spire.algebra.{Eq, Group}
import spire.util.Opt

import net.alasc.algebra.FaithfulPermutationAction

object BuildChain {

  def fromChain[G:ClassTag:Eq:Group](from: Chain[G], action: FaithfulPermutationAction[G], baseGuideOpt: Opt[BaseGuide] = Opt.empty[BaseGuide])(implicit baseChange: BaseChange, schreierSims: SchreierSims): Chain[G] = baseGuideOpt match {
    case Opt(baseGuide) => from match {
      case node: Node[G] if action == node.action =>
        if (baseGuide.isSatisfiedBy(from))
          from
        else {
          val mut = imply(action) { from.mutableChain }
          baseChange.changeBase(mut, baseGuide)
          mut.toChain
        }
      case node: Node[G] => // action != node.action
        val baseStart = baseGuide.baseAnsatz(node.strongGeneratingSet, action)
        val mut = schreierSims.completeChainChangeAction(node, action, baseStart)
        baseChange.changeBase(mut, baseGuide)
        mut.toChain
      case term: Term[G] => term
    }
    case _ => from match {
      case term: Term[G] => term
      case node: Node[G] if action == node.action => node
      case node: Node[G] => schreierSims.completeChainChangeAction(node, action).toChain()
    }
  }

  def fromGenerators[G:ClassTag:Eq:Group](generators: Iterable[G], action: FaithfulPermutationAction[G], baseGuideOpt: Opt[BaseGuide] = Opt.empty[BaseGuide])(implicit baseChange: BaseChange, schreierSims: SchreierSims): Chain[G] =
    BuildMutableChain.fromGenerators(generators, action, baseGuideOpt).toChain

  def fromGeneratorsAndOrder[G:ClassTag:Eq:Group](generators: Iterable[G], order: BigInt, action: FaithfulPermutationAction[G], baseGuideOpt: Opt[BaseGuide] = Opt.empty[BaseGuide])(implicit baseChange: BaseChange, schreierSims: SchreierSims): Chain[G] =
    BuildMutableChain.fromGeneratorsAndOrder(generators, order, action, baseGuideOpt).toChain

  def fromGeneratorsRandomElementsAndOrder[G:ClassTag:Eq:Group](generators: Iterable[G], randomElement: Random => G, order: BigInt, action: FaithfulPermutationAction[G], baseGuideOpt: Opt[BaseGuide] = Opt.empty[BaseGuide])(implicit baseChange: BaseChange, schreierSims: SchreierSims): Chain[G] =
    BuildMutableChain.fromGeneratorsRandomElementsAndOrder(generators, randomElement, order, action, baseGuideOpt).toChain

}
