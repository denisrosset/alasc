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

case class BaseGuideSet(val set: Set[Int]) extends BaseGuide {
  final class Iter(val remaining: mutable.BitSet) extends BaseGuideIterator {
    def hasNext = remaining.nonEmpty

    def next(beta: Int, easyPoints: collection.Set[Int], isFixed: Int => Boolean): Int =
      if (remaining.isEmpty) beta else {
        val toRemove = mutable.BitSet.empty
        def find: Int = {
          val inter = remaining intersect easyPoints
          inter.foreach { k =>
            toRemove += k
            if (!isFixed(k))
              return k
          }
          remaining --= toRemove
          toRemove.clear
          remaining.foreach { k =>
            toRemove += k
            if (!isFixed(k))
              return k
          }
          beta
        }
        val newPoint = find
        remaining --= toRemove
        newPoint
      }
    override def checksNext(beta: Int, isFixed: Int => Boolean): Boolean =
      if (remaining.isEmpty) true else
        if (remaining.contains(beta)) {
          remaining -= beta
          true
        } else {
          val allFixed = remaining.forall(isFixed(_))
          if (allFixed) {
            remaining.clear
            true
          } else false
        }
  }
  def fullBase = set.toSeq
  def iterator = new Iter(mutable.BitSet.empty ++= set)
}
