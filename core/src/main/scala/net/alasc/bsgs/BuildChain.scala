package net.alasc.bsgs

import scala.reflect.ClassTag
import scala.util.Random

import spire.algebra.{Eq, Group}
import spire.math.SafeLong
import spire.util.Opt

import net.alasc.algebra.PermutationAction

object BuildChain {

  def fromChain[G:ClassTag:Eq:Group, A1 <: PermutationAction[G] with Singleton, A2 <: PermutationAction[G] with Singleton]
  (from: Chain[G, A1], oldKernel: Chain.Generic[G], kb: KernelBuilder[G])(implicit newAction: A2, schreierSims: SchreierSims): Chain[G, A2] =
    from match {
      case node: Node[G, A1] if node.action eq newAction =>
        kb.replaceChain(oldKernel) // reuse same kb
        from.asInstanceOf[Node[G, A2]]
      case term: Term[G, A1] if oldKernel.strongGeneratingSet.forall(!newAction.movesAnyPoint(_)) =>
        // kb group is the same under the new action
        kb.replaceChain(oldKernel)
        term.asInstanceOf[Term[G, A2]]
      case _ => // different action
        BuildMutableChain.recomputeChain[G, A2](from, oldKernel, kb, Seq.empty).toChain()
    }

  def fromChain[G:ClassTag:Eq:Group, A1 <: PermutationAction[G] with Singleton, A2 <: PermutationAction[G] with Singleton]
    (from: Chain[G, A1], oldKernel: Chain.Generic[G], kb: KernelBuilder[G], baseGuideOpt: Opt[BaseGuide])
    (implicit newAction: A2, baseChange: BaseChange, schreierSims: SchreierSims): Chain[G, A2] = {
    val baseGuide = baseGuideOpt.getOrElseFast(BaseGuide.Empty)
    from match {
      case node: Node[G, A1] if node.action eq newAction =>
        kb.replaceChain(oldKernel) // reuse same kb
        if (baseGuide.isSatisfiedBy(from))
          from.asInstanceOf[Node[G, A2]]
        else {
          val mut = from.asInstanceOf[Node[G, A2]].mutableChain
          baseChange.changeBase(mut, kb.toChain(), baseGuide)
          mut.toChain()
        }
      case term: Term[G, A1] if oldKernel.strongGeneratingSet.forall(!newAction.movesAnyPoint(_)) =>
        // kb group is the same under the new action
        kb.replaceChain(oldKernel)
        term.asInstanceOf[Term[G, A2]]
      case _ => // different action
        val baseStart = baseGuide.baseAnsatz[G, A2](from.strongGeneratingSet ++ oldKernel.strongGeneratingSet)
        val mut = BuildMutableChain.recomputeChain[G, A2](from, oldKernel, kb, baseStart)
        baseChange.changeBase(mut, kb.toChain(), baseGuide)
        mut.toChain()
    }
  }

  def withBase[G:ClassTag:Eq:Group, F <: PermutationAction[G] with Singleton]
  (from: Chain[G, F], baseGuide: BaseGuide, oldKernel: Chain.Generic[G])(implicit action: F, baseChange: BaseChange): Chain[G, F] =
    if (baseGuide.isSatisfiedBy(from)) from else {
      val mut = from.mutableChain
      baseChange.changeBase(mut, oldKernel, baseGuide)
      mut.toChain()
    }

  def apply[G:ClassTag:Eq:Group, F <: PermutationAction[G] with Singleton]
    (generators: Iterable[G], kb: KernelBuilder[G])(implicit action: F, schreierSims: SchreierSims): Chain[G, F] =
    BuildMutableChain[G, F](generators, kb).toChain

  def apply[G:ClassTag:Eq:Group, F <: PermutationAction[G] with Singleton]
  (generators: Iterable[G], kb: KernelBuilder[G], baseGuideOpt: Opt[BaseGuide])
  (implicit action: F, baseChange: BaseChange, schreierSims: SchreierSims): Chain[G, F] =
    BuildMutableChain[G, F](generators, kb, baseGuideOpt).toChain()

  def apply[G:ClassTag:Eq:Group, F <: PermutationAction[G] with Singleton]
  (generators: Iterable[G], order: SafeLong, kb: KernelBuilder[G])
  (implicit action: F, schreierSims: SchreierSims): Chain[G, F] =
    BuildMutableChain[G, F](generators, order, kb).toChain()

  def apply[G:ClassTag:Eq:Group, F <: PermutationAction[G] with Singleton]
    (generators: Iterable[G], order: SafeLong, kb: KernelBuilder[G], baseGuideOpt: Opt[BaseGuide])
    (implicit action: F, baseChange: BaseChange, schreierSims: SchreierSims): Chain[G, F] =
    BuildMutableChain[G, F](generators, order, kb, baseGuideOpt).toChain()

  def apply[G:ClassTag:Eq:Group, F <: PermutationAction[G] with Singleton]
  (generators: Iterable[G], randomElement: Random => G, order: SafeLong, kb: KernelBuilder[G])(implicit action: F, schreierSims: SchreierSims): Chain[G, F] =
    BuildMutableChain[G, F](generators, randomElement, order, kb).toChain()

  def apply[G:ClassTag:Eq:Group, F <: PermutationAction[G] with Singleton]
    (generators: Iterable[G], randomElement: Random => G, order: SafeLong, kb: KernelBuilder[G], baseGuideOpt: Opt[BaseGuide])
    (implicit action: F, baseChange: BaseChange, schreierSims: SchreierSims): Chain[G, F] =
    BuildMutableChain[G, F](generators, randomElement, order, kb, baseGuideOpt).toChain()

}
