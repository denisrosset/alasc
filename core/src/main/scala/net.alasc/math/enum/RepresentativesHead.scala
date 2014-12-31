package net.alasc
package math
package enum

import scala.collection.mutable

import spire.algebra.{Eq,  Order}
import spire.syntax.group._
import spire.syntax.action._

import net.alasc.algebra._
import net.alasc.syntax.sequence._
import net.alasc.util._

import bsgs._

trait RepresentativesHead[T, G] extends RepresentativesOrdered[T, G] with coll.HasHead[Representative[T, G]] {
  self =>
  implicit def finiteGroupG: FiniteGroup[G]
  /** Returns the minimal lexicographic representative under permutation. */
  def head: LexRepresentative[T, G] = {
    val minG = Algorithms.findMinimalPermutation(tLength, tInt, chainInRepresentation, symGrp, representation)
    new LexRepresentative[T, G] {
      val element = minG.inverse
      val original = t
      implicit val actionTG = self.actionTG
      def rank = BigInt(0)
    }
  }
}
