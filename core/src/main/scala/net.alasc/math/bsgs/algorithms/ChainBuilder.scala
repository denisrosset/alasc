package net.alasc.math
package bsgs
package algorithms

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Random

import spire.syntax.action._

import net.alasc.algebra.FaithfulPermutationAction
import net.alasc.math.guide.{BaseGuide, BaseGuideSeq}
import net.alasc.syntax.permutationAction._

trait ChainBuilder[P] extends BaseChange[P] with SchreierSims[P] {
  /** Returns an incomplete base for the given generators, used when base has to be computed from scratch. */
  def baseAnsatz(baseGuide: BaseGuide, generators: Iterable[P])(implicit action: FaithfulPermutationAction[P]): Seq[Int] = {
    val base = mutable.ArrayBuffer.empty[Int]
    val it = baseGuide.iterator
    @tailrec def rec(remaining: Iterable[P]): Unit =
      if (remaining.nonEmpty && it.hasNext) {
        val sup = remaining.map(_.support).reduce(_ ++ _)
        if (sup.nonEmpty) {
          val beta = it.next(sup.min, sup, (k: Int) => remaining.forall(g => (k <|+| g) == k))
          base += beta
          rec(remaining.filter(g => (beta <|+| g) == beta))
        }
      }
    base.result
  }

  def chainWithBase(from: Chain[P], baseGuide: BaseGuide, action: FaithfulPermutationAction[P]): Chain[P] = from match {
    case node: Node[P] if action == node.action =>
      if (baseGuide.isSatisfiedBy(from)) from else {
        val mut = mutableChain(from)(action)
        changeBaseSameAction(mut, baseGuide)(action)
        mut.toChain
      }
    case node: Node[P] => // action != node.action
      val baseStart = baseAnsatz(baseGuide, node.strongGeneratingSet)(action)
      val mut = completeChainActionChange(node, action, baseStart)
      changeBaseSameAction(mut, baseGuide)(action)
      mut.toChain
    case _: Term[P] => from
  }

  def mutableChainWithBase(generators: Iterable[P], baseGuide: BaseGuide, action: FaithfulPermutationAction[P]): MutableChain[P] = {
    val mut = completeChainFromGenerators(generators, baseAnsatz(baseGuide, generators)(action))(action)
    changeBaseSameAction(mut, baseGuide)(action)
    mut
  }

  def mutableChainWithBase(generators: Iterable[P], order: BigInt, baseGuide: BaseGuide, action: FaithfulPermutationAction[P]): MutableChain[P] = {
    val mut = completeChainFromGeneratorsAndOrder(generators, order, baseAnsatz(baseGuide, generators)(action))(action)
    changeBaseSameAction(mut, baseGuide)(action)
    mut
  }

  def mutableChainWithBase(generators: Iterable[P], randomElement: Function1[Random, P], order: BigInt, baseGuide: BaseGuide, action: FaithfulPermutationAction[P]): MutableChain[P] = {
    val mut = completeChainFromGeneratorsRandomAndOrder(generators, randomElement, order, baseAnsatz(baseGuide, generators)(action))(action)
    changeBaseSameAction(mut, baseGuide)(action)
    mut
  }

  def chainWithBase(generators: Iterable[P], baseGuide: BaseGuide, action: FaithfulPermutationAction[P]): Chain[P] =
    mutableChainWithBase(generators, baseGuide, action).toChain


  def chainWithBase(generators: Iterable[P], order: BigInt, baseGuide: BaseGuide, action: FaithfulPermutationAction[P]): Chain[P] =
    mutableChainWithBase(generators, order, baseGuide, action).toChain

  def chainWithBase(generators: Iterable[P], randomElement: Function1[Random, P], order: BigInt, baseGuide: BaseGuide, action: FaithfulPermutationAction[P]): Chain[P] =
    mutableChainWithBase(generators, randomElement, order, baseGuide, action).toChain

  def mutableChainCopyWithBase(chain: Chain[P], baseGuide: BaseGuide)(implicit action: FaithfulPermutationAction[P]): MutableChain[P] = {
    val mutableChain = mutableChainCopyWithAction(chain, action)
    changeBaseSameAction(mutableChain, baseGuide)
    mutableChain
  }
  def mutableChainCopyWithBase(chain: Chain[P], base: Seq[Int])(implicit action: FaithfulPermutationAction[P]): MutableChain[P] =
    mutableChainCopyWithBase(chain, BaseGuideSeq(base))

  def withBase(chain: Chain[P], baseGuide: BaseGuide)(implicit action: FaithfulPermutationAction[P]): Chain[P] = chain match {
    case node: Node[P] if node.action == action && baseGuide.isSatisfiedBy(node) => node
    case _ => mutableChainCopyWithBase(chain, baseGuide).toChain
  }
  def withBase(chain: Chain[P], base: Seq[Int])(implicit action: FaithfulPermutationAction[P]): Chain[P] =
    withBase(chain, BaseGuideSeq(base))

  def withAction(chain: Chain[P], action: FaithfulPermutationAction[P]): Chain[P] = chain match {
    case term: Term[P] => term
    case node: Node[P] if action == node.action => node
    case node: Node[P] => completeChainActionChange(node, action).toChain
  }
  
  def mutableChainCopyWithAction(chain: Chain[P], action: FaithfulPermutationAction[P]): MutableChain[P] = chain match {
    case node: Node[P] if action == node.action => mutableChain(node)(action)
    case node: Node[P] => completeChainActionChange(chain, action)
    case _: Term[P] => MutableChain.empty(group, action)
  }
}
