package net.alasc.math
package bsgs
package algorithms

import scala.annotation.tailrec

import scala.collection.immutable.BitSet
import scala.collection.immutable
import scala.collection.mutable

import spire.algebra.Order
import spire.syntax.groupAction._
import spire.syntax.group._

import net.alasc.algebra.{FaithfulPermutationAction, Subgroup}
import net.alasc.syntax.check._
import net.alasc.util._

trait BaseChangeFromScratch[P] extends BaseChange[P] with SchreierSims[P] {
  def changeBaseSameAction(mutableChain: MutableChain[P], baseGuide: BaseGuide)(
    implicit action: FaithfulPermutationAction[P]): Unit = {
    require(action == mutableChain.start.action)
    val tempChain = completeChainFromSubgroup(mutableChain.start.next, baseGuide.fullBase)(mutableChain.start.action, implicitly[Subgroup[Chain[P], P]])
    mutableChain.replaceChain(mutableChain.start, tempChain.start)
  }
}
