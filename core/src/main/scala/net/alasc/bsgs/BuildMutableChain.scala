package net.alasc.bsgs

import scala.reflect.ClassTag
import scala.util.Random

import spire.algebra.{Eq, Group}
import spire.math.SafeLong
import spire.syntax.group._
import spire.util.Opt

import net.alasc.algebra.PermutationAction

object BuildMutableChain {

  def recomputeChain[G:ClassTag:Eq:Group, A <: PermutationAction[G] with Singleton]
  (from: Chain.Generic[G], oldKernel: Chain.Generic[G], kb: KernelBuilder[G], baseStart: Seq[Int])(implicit newAction: A, schreierSims: SchreierSims): MutableChain[G, A] = {
    val generators = from.strongGeneratingSet ++ oldKernel.strongGeneratingSet
    def randomElement(rand: Random): G = from.randomElement(rand) |+| oldKernel.randomElement(rand)
    val order = from.order * oldKernel.order
    schreierSims.mutableChain[G, A](generators, randomElement(_), order, kb, baseStart)
  }

  def fromChain[G:ClassTag:Eq:Group, A1 <: PermutationAction[G] with Singleton, A2 <: PermutationAction[G] with Singleton]
  (from: Chain[G, A1], oldKernel: Chain.Generic[G], kb: KernelBuilder[G])(implicit newAction: A2, schreierSims: SchreierSims): MutableChain[G, A2] =
    from match {
      case node: Node[G, A1] if node.action eq newAction =>
        kb.replaceChain(oldKernel)
        node.asInstanceOf[Node[G, A2]].mutableChain
      case term: Term[G, A1] if oldKernel.strongGeneratingSet.forall(!newAction.movesAnyPoint(_)) =>
        // kb group is the same under the new action
        kb.replaceChain(oldKernel)
        MutableChain.empty[G, A2]
      case _ => // different action
        recomputeChain[G, A2](from, oldKernel, kb, Seq.empty)
    }

  def fromChain[G:ClassTag:Eq:Group, A1 <: PermutationAction[G] with Singleton, A2 <: PermutationAction[G] with Singleton]
  (from: Chain[G, A1], oldKernel: Chain.Generic[G], kb: KernelBuilder[G], baseGuideOpt: Opt[BaseGuide])
  (implicit newAction: A2, baseChange: BaseChange, schreierSims: SchreierSims): MutableChain[G, A2] = {
    val baseGuide = baseGuideOpt.getOrElseFast(BaseGuide.Empty)
    val mutableChain = from match {
      case node: Node[G, A1] if node.action eq newAction =>
        kb.replaceChain(oldKernel)
        node.asInstanceOf[Node[G, A2]].mutableChain
      case term: Term[G, A1] if oldKernel.strongGeneratingSet.forall(!newAction.movesAnyPoint(_)) =>
        // kb group is the same under the new action
        kb.replaceChain(oldKernel)
        MutableChain.empty[G, A2]
      case _ => // different action
        val baseStart = baseGuide.baseAnsatz[G, A2](from.strongGeneratingSet ++ oldKernel.strongGeneratingSet)
        recomputeChain[G, A2](from, oldKernel, kb, baseStart)
    }
    val kernelChain = kb.toChain()
    baseChange.changeBase[G, A2](mutableChain, kernelChain, baseGuide)
    mutableChain
  }

  def apply[G:ClassTag:Eq:Group, A <: PermutationAction[G] with Singleton](generators: Iterable[G], kb: KernelBuilder[G])(implicit action: A, schreierSims: SchreierSims): MutableChain[G, A] =
    schreierSims.mutableChain[G, A](generators, kb, Seq.empty)


  def apply[G:ClassTag:Eq:Group, A <: PermutationAction[G] with Singleton](generators: Iterable[G], kb: KernelBuilder[G], baseGuideOpt: Opt[BaseGuide])
  (implicit action: A, baseChange: BaseChange, schreierSims: SchreierSims): MutableChain[G, A] = {
    val baseGuide = baseGuideOpt.getOrElseFast(BaseGuide.Empty)
    val ansatz = baseGuide.baseAnsatz[G, A](generators)
    val mutableChain = schreierSims.mutableChain[G, A](generators, kb, ansatz)
    baseChange.changeBase(mutableChain, kb.toChain(), baseGuide)
    mutableChain
  }

  def apply[G:ClassTag:Eq:Group, A <: PermutationAction[G] with Singleton]
  (generators: Iterable[G], order: SafeLong, kb: KernelBuilder[G])(implicit action: A, schreierSims: SchreierSims): MutableChain[G, A] =
    schreierSims.mutableChain[G, A](generators, order, kb, Seq.empty)

  def apply[G:ClassTag:Eq:Group, A <: PermutationAction[G] with Singleton]
    (generators: Iterable[G], order: SafeLong, kb: KernelBuilder[G], baseGuideOpt: Opt[BaseGuide])
    (implicit action: A, baseChange: BaseChange, schreierSims: SchreierSims): MutableChain[G, A] = {
    val baseGuide = baseGuideOpt.getOrElseFast(BaseGuide.Empty)
    val ansatz = baseGuide.baseAnsatz[G, A](generators)
    val mutableChain = schreierSims.mutableChain[G, A](generators, order, kb, ansatz)
    baseChange.changeBase(mutableChain, kb.toChain(), baseGuide)
    mutableChain
  }

  def apply[G:ClassTag:Eq:Group, A <: PermutationAction[G] with Singleton]
  (generators: Iterable[G], randomElement: Random => G, order: SafeLong, kb: KernelBuilder[G])(implicit action: A, schreierSims: SchreierSims): MutableChain[G, A] =
    schreierSims.mutableChain[G, A](generators, randomElement, order, kb, Seq.empty)


  def apply[G:ClassTag:Eq:Group, A <: PermutationAction[G] with Singleton]
    (generators: Iterable[G], randomElement: Random => G, order: SafeLong, kb: KernelBuilder[G], baseGuideOpt: Opt[BaseGuide])
    (implicit action: A, baseChange: BaseChange, schreierSims: SchreierSims): MutableChain[G, A] = {
    val baseGuide = baseGuideOpt.getOrElseFast(BaseGuide.Empty)
    val ansatz = baseGuide.baseAnsatz[G, A](generators)
    val mutableChain = schreierSims.mutableChain[G, A](generators, randomElement, order, kb, ansatz)
    baseChange.changeBase(mutableChain, kb.toChain(), baseGuide)
    mutableChain
  }

}
