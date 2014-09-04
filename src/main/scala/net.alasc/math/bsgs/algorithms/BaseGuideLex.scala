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

case class BaseGuideLex(n: Int) extends BaseGuide {
  final class Iter(var k: Int) extends BaseGuideIterator {
    def hasNext = (k != n)

    @tailrec def next(beta: Int, easyPoints: collection.Set[Int], isFixed: Int => Boolean): Int =
      if (k == n)
        beta
      else if (isFixed(k)) {
        k += 1
        next(beta, easyPoints, isFixed)
      } else {
        val res = k
        k += 1
        res
      }

    override def checksNext(beta: Int, isFixed: Int => Boolean): Boolean =
      if (k == n)
        true
      else {
        if (k > beta)
          return false
        while (k < beta) {
          if (!isFixed(k))
            return false
          k += 1
        }
        true
      }
  }
  def fullBase = Seq.tabulate(n)(identity)
  def iterator = new Iter(0)
}
