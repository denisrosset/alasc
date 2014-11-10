package net.alasc.math
package bsgs
package algorithms

import net.alasc.algebra.{FaithfulPermutationAction, Subgroup}
import net.alasc.math.guide.BaseGuide

trait BaseChangeFromScratch[P] extends BaseChange[P] with SchreierSims[P] {
  def changeBaseSameAction(mutableChain: MutableChain[P], baseGuide: BaseGuide)(
    implicit action: FaithfulPermutationAction[P]): Unit = {
    require(action == mutableChain.start.action)
    val tempChain = completeChainFromSubgroup(mutableChain.start.next, baseGuide.fullBase)(mutableChain.start.action, implicitly[Subgroup[Chain[P], P]])
    mutableChain.replaceChain(mutableChain.start, tempChain.start)
  }
}
