package net.alasc
package math
package enum

import scala.collection.mutable

import spire.algebra.{Eq, Group, Order}
import spire.syntax.group._
import spire.syntax.action._
import spire.util._

import net.alasc.algebra._
import net.alasc.util._

import bsgs._
import bsgs.algorithms._
import guide._

trait RepresentativesSearchable[T, G] extends Representatives[T, G] {
  implicit def T: EnumerableSearchable[T]
  implicit def algorithms: BasicAlgorithms[G] = grp.algorithms
  def chainInRepresentation = grp.chain(representation, BaseGuideLex(T.size(t)))
  def chainInRepresentationBasePointGroups = algorithms.basePointGroups(chainInRepresentation, representation.size)
  def findRepresentative(some: T): Opt[Representative[T, G]]
}

final class RepresentativesSearchableImpl[T, G](val t: T, val grp: Grp[G])(implicit val T: EnumerableSearchable[T], val TG: Permutable[T, G]) extends RepresentativesSearchable[T, G] {
  override lazy val chainInRepresentation = super.chainInRepresentation
  override lazy val chainInRepresentationBasePointGroups = super.chainInRepresentationBasePointGroups
  override lazy val partition = super.partition
  override lazy val symGrp = super.symGrp
  def findRepresentative(r: T): Opt[Representative[T, G]] =
    T.commonPartitions(t, r).flatMap { cp =>
      import grp.finiteGroup
      val cpSeq: Seq[(Set[Int], Set[Int])] = cp.blocks
      val n = T.size(t)
      val bo = bsgs.algorithms.BaseOrder[G](chainInRepresentation.base)(representation.action)
      val tIntArray = new Array[Int](n)
      val rIntArray = new Array[Int](n)
      cp.blocks.zipWithIndex.foreach {
        case ((tInd, rInd), i) =>
          tInd.foreach { tIntArray(_) = i }
          rInd.foreach { rIntArray(_) = i }
      }
      def rec(level: Int, g: G, chainGrp: Chain[G], chainSym: Grp[G]): Opt[Representative[T, G]] = chainGrp match {
        case node: Node[G] =>
          implicit def action = representation.action
          val orbitIt = node.orbit.iterator
          val beta = node.beta
          while (orbitIt.hasNext) {
            val b = orbitIt.next
            val bg = action.actr(b, g)
            if (rIntArray(beta) == tIntArray(bg)) {
              val nextG = node.u(b) |+| g
              var j = 1
              var disagree = false
              val m = chainInRepresentationBasePointGroups(level).length
              while (j < m && !disagree) {
                val c = chainInRepresentationBasePointGroups(level)(j)
                if (rIntArray(c) != tIntArray(c <|+| nextG))
                  disagree = true
                j += 1
              }
              if (!disagree) {
                val (nextSym, transversal) = chainSym.stabilizer(bg, representation)
                if (transversal.orbit.min(Order.ordering(bo)) == bg) {
                  val res = rec(level + 1, nextG, node.next, nextSym)
                  if (res.nonEmpty)
                    return res
                }
              }
            }
          }
          Opt.empty[Representative[T, G]]
        case _: Term[G] => Opt(Representative(t, g.inverse))
      }
      rec(0, Group[G].id, chainInRepresentation, symGrp)
    }
}
