package net.alasc.bsgs

import scala.reflect.ClassTag
import scala.util.Random

import spire.algebra.{Eq, Group}
import spire.math.SafeLong
import spire.util.Opt

import net.alasc.algebra.FaithfulPermutationAction

object BuildMutableChain {

  protected def shapeAndReturn[G:ClassTag:Eq:Group, F <: FaithfulPermutationAction[G] with Singleton]
    (mutableChain: MutableChain[G, F], baseGuideOpt: Opt[BaseGuide])
    (implicit baseChange: BaseChange): MutableChain[G, F] = baseGuideOpt match {
    case Opt(baseGuide) =>
      baseChange.changeBase(mutableChain, baseGuide)
      mutableChain
    case _ => mutableChain
  }

  def fromChain[G:ClassTag:Eq:Group, F1 <: FaithfulPermutationAction[G] with Singleton, F2 <: FaithfulPermutationAction[G] with Singleton]
    (from: Chain[G, F1], baseGuideOpt: Opt[BaseGuide] = Opt.empty[BaseGuide])
    (implicit newAction: F2, baseChange: BaseChange, schreierSims: SchreierSims): MutableChain[G, F2] = {
    val mut = from match {
      case node: Node[G, F1] =>
        if (node.action eq newAction)
          node.asInstanceOf[Node[G, F2]].mutableChain
        else {
          val baseStart = baseGuideOpt.getOrElseFast(BaseGuide.empty).baseAnsatz[G, F2](from.strongGeneratingSet)
          schreierSims.completeChainChangeAction(from, newAction: F2, baseStart)
        }
      case _: Term[G, F1] => MutableChain.empty[G, F2]
    }
    shapeAndReturn[G, F2](mut, baseGuideOpt)
  }

  def fromGenerators[G:ClassTag:Eq:Group, F <: FaithfulPermutationAction[G] with Singleton]
    (generators: Iterable[G], baseGuideOpt: Opt[BaseGuide] = Opt.empty[BaseGuide])
    (implicit action: F, baseChange: BaseChange, schreierSims: SchreierSims): MutableChain[G, F] = {
    val ansatz = baseGuideOpt.getOrElseFast(BaseGuide.empty).baseAnsatz[G, F](generators)
    val mut = schreierSims.completeChainFromGenerators[G, F](generators, ansatz)
    shapeAndReturn[G, F](mut, baseGuideOpt)
  }

  def fromGeneratorsAndOrder[G:ClassTag:Eq:Group, F <: FaithfulPermutationAction[G] with Singleton]
    (generators: Iterable[G], order: SafeLong, baseGuideOpt: Opt[BaseGuide] = Opt.empty[BaseGuide])
    (implicit action: F, baseChange: BaseChange, schreierSims: SchreierSims): MutableChain[G, F] = {
    val ansatz = baseGuideOpt.getOrElseFast(BaseGuide.empty).baseAnsatz[G, F](generators)
    val mut = schreierSims.completeChainFromGeneratorsAndOrder[G, F](generators, order, ansatz)
    shapeAndReturn[G, F](mut, baseGuideOpt)
  }

  def fromGeneratorsRandomElementsAndOrder[G:ClassTag:Eq:Group, F <: FaithfulPermutationAction[G] with Singleton]
    (generators: Iterable[G], randomElement: Random => G, order: SafeLong, baseGuideOpt: Opt[BaseGuide] = Opt.empty[BaseGuide])
    (implicit action: F, baseChange: BaseChange, schreierSims: SchreierSims): MutableChain[G, F] = {
    val ansatz = baseGuideOpt.getOrElseFast(BaseGuide.empty).baseAnsatz[G, F](generators)
    val mut = schreierSims.completeChainFromGeneratorsRandomElementsAndOrder[G, F](generators, randomElement, order, ansatz)
    shapeAndReturn[G, F](mut, baseGuideOpt)
  }

}
