package net.alasc.bsgs

import scala.reflect.ClassTag
import scala.util.Random

import spire.algebra.{Eq, Group}
import spire.util.Opt

import net.alasc.algebra.FaithfulPermutationAction

object BuildMutableChain {

  protected def shapeAndReturn[G:ClassTag:Eq:Group](mutableChain: MutableChain[G], baseGuideOpt: Opt[BaseGuide])(implicit baseChange: BaseChange): MutableChain[G] = baseGuideOpt match {
    case Opt(baseGuide) =>
      baseChange.changeBase(mutableChain, baseGuide)
      mutableChain
    case _ => mutableChain
  }

  def fromChain[G:ClassTag:Eq:Group](from: Chain[G], action: FaithfulPermutationAction[G], baseGuideOpt: Opt[BaseGuide] = Opt.empty[BaseGuide])(implicit baseChange: BaseChange, schreierSims: SchreierSims): MutableChain[G] = {
    val mut = from match {
      case node: Node[G] if action == node.action => imply(action) { node.mutableChain }
      case node: Node[G] =>
        val baseStart = baseGuideOpt.baseAnsatz(from.strongGeneratingSet, action)
        schreierSims.completeChainChangeAction(from, action, baseStart)
      case _: Term[G] => imply(action) { MutableChain.empty[G] }
    }
    shapeAndReturn(mut, baseGuideOpt)
  }

  def fromGenerators[G:ClassTag:Eq:Group](generators: Iterable[G], action: FaithfulPermutationAction[G], baseGuideOpt: Opt[BaseGuide] = Opt.empty[BaseGuide])(implicit baseChange: BaseChange, schreierSims: SchreierSims): MutableChain[G] = {
    val ansatz = baseGuideOpt.baseAnsatz(generators, action)
    val mut = imply(action) { schreierSims.completeChainFromGenerators(generators, ansatz) }
    shapeAndReturn(mut, baseGuideOpt)
  }

  def fromGeneratorsAndOrder[G:ClassTag:Eq:Group](generators: Iterable[G], order: BigInt, action: FaithfulPermutationAction[G], baseGuideOpt: Opt[BaseGuide] = Opt.empty[BaseGuide])(implicit baseChange: BaseChange, schreierSims: SchreierSims): MutableChain[G] = {
    val ansatz = baseGuideOpt.baseAnsatz(generators, action)
    val mut = imply(action) {
      schreierSims.completeChainFromGeneratorsAndOrder(generators, order, ansatz)
    }
    shapeAndReturn(mut, baseGuideOpt)
  }

  def fromGeneratorsRandomElementsAndOrder[G:ClassTag:Eq:Group](generators: Iterable[G], randomElement: Random => G, order: BigInt, action: FaithfulPermutationAction[G], baseGuideOpt: Opt[BaseGuide] = Opt.empty[BaseGuide])(implicit baseChange: BaseChange, schreierSims: SchreierSims): MutableChain[G] = {
    val ansatz = baseGuideOpt.baseAnsatz(generators, action)
    val mut = imply(action) {
      schreierSims.completeChainFromGeneratorsRandomElementsAndOrder(generators, randomElement, order, ansatz)
    }
    shapeAndReturn(mut, baseGuideOpt)
  }

}
