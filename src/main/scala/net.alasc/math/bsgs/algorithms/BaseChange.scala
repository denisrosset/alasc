package net.alasc.math
package bsgs
package algorithms

import scala.annotation.tailrec

import scala.collection.BitSet
import scala.collection.immutable
import scala.collection.mutable

import spire.algebra.Order
import spire.syntax.groupAction._
import spire.syntax.group._

import net.alasc.algebra.{FaithfulPermutationAction, Subgroup}
import net.alasc.syntax.check._
import net.alasc.util._

trait BaseChange[P] extends SchreierSims[P] {
  /** Change the base in `mutableChain` using the base guide provided. Must not change the action.
    * 
    * @param mutableChain Mutable chain on which to perform the base change.
    * @param baseGuide    Guide for the new base.
    */
  def changeBaseSameAction(mutableChain: MutableChain[P], baseGuide: BaseGuide)(implicit action: FaithfulPermutationAction[P]): Unit
  def changeBaseSameAction(mutableChain: MutableChain[P], base: Seq[Int])(implicit action: FaithfulPermutationAction[P]): Unit =
    changeBaseSameAction(mutableChain, BaseGuideSeq(base))

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
}
