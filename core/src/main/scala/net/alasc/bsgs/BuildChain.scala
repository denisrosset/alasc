package net.alasc.bsgs

import scala.reflect.ClassTag
import scala.util.Random

import spire.algebra.{Eq, Group}
import spire.math.SafeLong
import spire.syntax.group._
import spire.util.Opt

import net.alasc.algebra.PermutationAction
import net.alasc.finite.Grp

object BuildChain {

  /*
  def fromChain[G:ClassTag:Eq:Group, F1 <: PermutationAction[G] with Singleton, F2 <: PermutationAction[G] with Singleton]
  (from: Chain[G, F1], oldKernel: Grp[G], kernel: KernelBuilder[G])(implicit newAction: F2, schreierSims: SchreierSims): Chain[G, F2] =
    from match {
      case node: Node[G, F1] if node.action eq newAction =>
        kernel.absorb(oldKernel) // reuse same kernel
        from.asInstanceOf[Node[G, F2]]
      case term: Term[G, F1] if oldKernel.generators.forall(!newAction.movesAnyPoint(_)) =>
        // kernel group is the same under the new action
        kernel.absorb(oldKernel)
        term.asInstanceOf[Term[G, F2]]
      case _ => // different action
        BuildMutableChain.fromChainAndKernel[G, F2](from, oldKernel, kernel).toChain()
    }

  def fromChain[G:ClassTag:Eq:Group, F1 <: PermutationAction[G] with Singleton, F2 <: PermutationAction[G] with Singleton]
    (from: Chain[G, F1], oldKernel: Grp[G], kernel: KernelBuilder[G], baseGuideOpt: Opt[BaseGuide] = Opt.empty[BaseGuide])
    (implicit newAction: F2, baseChange: BaseChange, schreierSims: SchreierSims): Chain[G, F2] = {
    val baseGuide = baseGuideOpt.getOrElseFast(BaseGuide.Empty)
    from match {
      case node: Node[G, F1] if node.action eq newAction =>
        kernel.absorb(oldKernel) // reuse same kernel
        if (baseGuide.isSatisfiedBy(from))
          from.asInstanceOf[Node[G, F2]]
        else {
          val mut = from.asInstanceOf[Node[G, F2]].mutableChain
          baseChange.changeBase(mut, baseGuide)
          mut.toChain()
        }
      case term: Term[G, F1] if oldKernel.generators.forall(!newAction.movesAnyPoint(_)) =>
        // kernel group is the same under the new action
        kernel.absorb(oldKernel)
        term.asInstanceOf[Term[G, F2]]
      case _ => // different action
        val baseStart = baseGuide.baseAnsatz[G, F2](from.strongGeneratingSet ++ oldKernel.generators)
        val mut = BuildMutableChain.fromChainAndKernel[G, F2](from, oldKernel, kernel, baseStart)
        baseChange.changeBase(mut, baseGuide)
        mut.toChain()
    }
  }

  def withBase[G:ClassTag:Eq:Group, F <: PermutationAction[G] with Singleton]
  (from: Chain[G, F], baseGuide: BaseGuide)(implicit action: F, baseChange: BaseChange): Chain[G, F] =
    if (baseGuide.isSatisfiedBy(from)) from else {
      val mut = from.mutableChain
      baseChange.changeBase(mut, baseGuide)
      mut.toChain()
    }

  def fromGenerators[G:ClassTag:Eq:Group, F <: PermutationAction[G] with Singleton]
    (generators: Iterable[G], kernel: KernelBuilder[G])(implicit action: F, schreierSims: SchreierSims): Chain[G, F] =
    BuildMutableChain.fromGenerators[G, F](generators, kernel).toChain

  def fromGenerators[G:ClassTag:Eq:Group, F <: PermutationAction[G] with Singleton]
  (generators: Iterable[G], kernel: KernelBuilder[G], baseGuideOpt: Opt[BaseGuide])
  (implicit action: F, baseChange: BaseChange, schreierSims: SchreierSims): Chain[G, F] =
    BuildMutableChain.fromGenerators[G, F](generators, kernel, baseGuideOpt).toChain()

  def fromGeneratorsAndOrder[G:ClassTag:Eq:Group, F <: PermutationAction[G] with Singleton]
  (generators: Iterable[G], order: SafeLong, kernel: KernelBuilder[G])
  (implicit action: F, schreierSims: SchreierSims): Chain[G, F] =
    BuildMutableChain.fromGeneratorsAndOrder[G, F](generators, order, kernel).toChain()

  def fromGeneratorsAndOrder[G:ClassTag:Eq:Group, F <: PermutationAction[G] with Singleton]
    (generators: Iterable[G], order: SafeLong, kernel: KernelBuilder[G], baseGuideOpt: Opt[BaseGuide])
    (implicit action: F, baseChange: BaseChange, schreierSims: SchreierSims): Chain[G, F] =
    BuildMutableChain.fromGeneratorsAndOrder[G, F](generators, order, kernel, baseGuideOpt).toChain()

  def fromGeneratorsRandomElementsAndOrder[G:ClassTag:Eq:Group, F <: PermutationAction[G] with Singleton]
  (generators: Iterable[G], randomElement: Random => G, order: SafeLong, kernel: KernelBuilder[G])(implicit action: F, schreierSims: SchreierSims): Chain[G, F] =
    BuildMutableChain.fromGeneratorsRandomElementsAndOrder[G, F](generators, randomElement, order, kernel).toChain()

  def fromGeneratorsRandomElementsAndOrder[G:ClassTag:Eq:Group, F <: PermutationAction[G] with Singleton]
    (generators: Iterable[G], randomElement: Random => G, order: SafeLong, kernel: KernelBuilder[G], baseGuideOpt: Opt[BaseGuide])
    (implicit action: F, baseChange: BaseChange, schreierSims: SchreierSims): Chain[G, F] =
    BuildMutableChain.fromGeneratorsRandomElementsAndOrder[G, F](generators, randomElement, order, kernel, baseGuideOpt).toChain()
*/
}
