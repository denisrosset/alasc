package net.alasc.math
package bsgs
package algorithms

import net.alasc.algebra.FaithfulPermutationAction
import net.alasc.math.guide.BaseGuide

trait BaseChangeFromScratch[P] extends BaseChange[P] with SchreierSims[P] {

  def changeBaseSameAction(mutableChain: MutableChain[P], baseGuide: BaseGuide)(
    implicit action: FaithfulPermutationAction[P]): Unit = {
    require(action == mutableChain.start.action)
    val oldChain = mutableChain.start.next
    val tempChain = completeChainFromGeneratorsRandomAndOrder(oldChain.strongGeneratingSet, oldChain.randomElement(_), oldChain.order, baseGuide.fullBase)(mutableChain.start.action)
    mutableChain.replaceChain(mutableChain.start, tempChain.start)
  }

}
