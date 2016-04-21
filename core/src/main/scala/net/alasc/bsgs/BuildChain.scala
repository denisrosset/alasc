package net.alasc.bsgs

import scala.reflect.ClassTag
import scala.util.Random

import spire.algebra.{Eq, Group}
import spire.math.SafeLong
import spire.util.Opt

import net.alasc.algebra.PermutationAction

object BuildChain {

  def fromChain[G:ClassTag:Eq:Group, F1 <: PermutationAction[G] with Singleton, F2 <: PermutationAction[G] with Singleton]
    (from: Chain[G, F1], baseGuideOpt: Opt[BaseGuide] = Opt.empty[BaseGuide])
    (implicit newAction: F2, baseChange: BaseChange, schreierSims: SchreierSims): Chain[G, F2] = baseGuideOpt match {
    case Opt(baseGuide) => from match {
      case node: Node[G, F1] =>
        if (node.action eq newAction) { // same action
          if (baseGuide.isSatisfiedBy(from))
            from.asInstanceOf[Node[G, F2]]
          else {
            val mut = from.asInstanceOf[Node[G, F2]].mutableChain
            baseChange.changeBase(mut, baseGuide)
            mut.toChain
          }
        } else { // new action
          val baseStart = baseGuide.baseAnsatz[G, F2](node.strongGeneratingSet)
          val mut = schreierSims.completeChainChangeAction(node, newAction: F2, baseStart)
          baseChange.changeBase(mut, baseGuide)
          mut.toChain
        }
      case term: Term[G, F1] => term.asInstanceOf[Term[G, F2]]
    }
    case _ => from match {
      case term: Term[G, F1] => term.asInstanceOf[Term[G, F2]]
      case node: Node[G, F1] =>
        if (newAction eq node.action)
          node.asInstanceOf[Node[G, F2]] // same action
        else
          schreierSims.completeChainChangeAction(node, newAction: F2).toChain() // new action
    }
  }

  def fromGenerators[G:ClassTag:Eq:Group, F <: PermutationAction[G] with Singleton]
    (generators: Iterable[G], baseGuideOpt: Opt[BaseGuide] = Opt.empty[BaseGuide])
    (implicit action: F, baseChange: BaseChange, schreierSims: SchreierSims): Chain[G, F] =
    BuildMutableChain.fromGenerators[G, F](generators, baseGuideOpt).toChain

  def fromGeneratorsAndOrder[G:ClassTag:Eq:Group, F <: PermutationAction[G] with Singleton]
    (generators: Iterable[G], order: SafeLong, baseGuideOpt: Opt[BaseGuide] = Opt.empty[BaseGuide])
    (implicit action: F, baseChange: BaseChange, schreierSims: SchreierSims): Chain[G, F] =
    BuildMutableChain.fromGeneratorsAndOrder[G, F](generators, order, baseGuideOpt).toChain

  def fromGeneratorsRandomElementsAndOrder[G:ClassTag:Eq:Group, F <: PermutationAction[G] with Singleton]
    (generators: Iterable[G], randomElement: Random => G, order: SafeLong, baseGuideOpt: Opt[BaseGuide] = Opt.empty[BaseGuide])
    (implicit action: F, baseChange: BaseChange, schreierSims: SchreierSims): Chain[G, F] =
    BuildMutableChain.fromGeneratorsRandomElementsAndOrder[G, F](generators, randomElement, order, baseGuideOpt).toChain

}
