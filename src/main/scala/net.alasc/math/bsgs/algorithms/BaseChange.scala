package net.alasc.math
package bsgs
package algorithms

import net.alasc.algebra.FaithfulPermutationAction
import net.alasc.math.guide.{BaseGuide, BaseGuideSeq}

trait BaseChange[P] extends SchreierSims[P] {
  /** Change the base in `mutableChain` using the base guide provided. Must not change the action.
    * 
    * Redundant base points are *not* inserted at the end of the chain.
    * 
    * @param mutableChain Mutable chain on which to perform the base change.
    * @param baseGuide    Guide for the new base.
    */
  def changeBaseSameAction(mutableChain: MutableChain[P], baseGuide: BaseGuide)(implicit action: FaithfulPermutationAction[P]): Unit
  def changeBaseSameAction(mutableChain: MutableChain[P], base: Seq[Int])(implicit action: FaithfulPermutationAction[P]): Unit =
    changeBaseSameAction(mutableChain, BaseGuideSeq(base))
}
