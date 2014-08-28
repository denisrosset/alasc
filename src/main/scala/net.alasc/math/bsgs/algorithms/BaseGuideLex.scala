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

final class BaseGuideLex(var next: Int, val n: Int) extends BaseGuide {
  def remainingBase = (next until n).toSeq
  def hasAdvice = (next != n)
  @tailrec def basePoint(beta: Int, easyPoints: collection.Set[Int], isFixed: Int => Boolean): Int =
    if (next == n)
      beta
    else if (isFixed(next)) {
      next += 1
      basePoint(beta, easyPoints, isFixed)
    } else
      next

  def moveToNext(chosenPoint: Int) = {
    assert(chosenPoint <= next)
    if (chosenPoint == next)
      next += 1
  }
}

object BaseGuideLex {
  def apply(n: Int) = new BaseGuideLex(0, n)
}
