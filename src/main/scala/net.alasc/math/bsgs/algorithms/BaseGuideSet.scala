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

import net.alasc.algebra.{PermutationAction, Subgroup}
import net.alasc.syntax.check._
import net.alasc.util._

object BaseGuideSet {
  def apply(set: Set[Int]) = new BaseGuideSet(mutable.BitSet.empty ++= set)
}

final class BaseGuideSet(val set: mutable.BitSet) extends BaseGuide {
  def remainingBase = set.toSeq
  def hasAdvice = set.nonEmpty
  def basePoint(beta: Int, easyPoints: collection.Set[Int], isFixed: Int => Boolean): Int = {
    val inter = set intersect easyPoints
    if (inter.nonEmpty) inter.head else {
      val toRemove = mutable.BitSet.empty
      var res = NNNone
      set.foreach { k =>
        if (isFixed(k))
          toRemove += k
        else if (res.isEmpty)
          res = NNSome(k)
      }
      set --= toRemove
      res match {
        case NNOption(k) => k
        case _ => beta
      }
    }
  }
  
  def moveToNext[P](chosenPoint: Int) = {
    assert(set.contains(chosenPoint) || set.isEmpty)
    set -= chosenPoint
  }
}
