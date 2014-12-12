package net.alasc
package math
package enum

import scala.collection.mutable

import spire.algebra.{Eq, Action, Order}
import spire.syntax.group._
import spire.syntax.action._

import net.alasc.algebra._
import net.alasc.syntax.sequence._
import net.alasc.util._

import bsgs._

trait RepresentativesSearchable[T, G] extends Representatives[T, G] {
  self =>
  import grp.{algorithms, representation}
  def chainInRepresentation: Chain[G]
  lazy val chainInRepresentationBasePointGroups = algorithms.basePointGroups(chainInRepresentation, representation.size)
  def find(r: T): Option[Representative[T, G]] = {
    val tIntArray = Array.tabulate(tLength)(tInt(_))
    val bo = bsgs.algorithms.BaseOrder[G](chainInRepresentation.base)(representation.action)
    val rIntArray = new Array[Int](tLength)
    var idx = 0
    while (idx < tLength) {
      seqInt(r, idx) match {
        case NNOption(i) => rIntArray(idx) = i
        case _ => return None
      }
      idx += 1
    }
    def rec(level: Int, g: G, chainGrp: Chain[G], chainSym: Grp[G]): Option[Representative[T, G]] = chainGrp match {
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
        None
      case _: Term[G] => Some(new Representative[T, G] {
        val element = g.inverse
        val original = t
        implicit val actionTG = self.actionTG
        })
    }
    rec(0, finiteGroupG.id, chainInRepresentation, symGrp)
  }
}
