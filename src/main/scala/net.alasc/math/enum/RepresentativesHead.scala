package net.alasc
package math
package enum

import scala.collection.mutable

import spire.algebra.{Eq, GroupAction, Order}
import spire.syntax.group._
import spire.syntax.groupAction._

import net.alasc.algebra._
import net.alasc.syntax.sequence._
import net.alasc.util._

import bsgs._

trait RepresentativesHead[T, G] extends RepresentativesOrdered[T, G] with coll.HasHead[Representative[T, G]] {
  self =>
  implicit def finiteGroupG: FiniteGroup[G]
  /** Returns the minimal lexicographic representative under permutation. */
  def head: LexRepresentative[T, G] = {
    // Implements breadth-first search in the cosets `symGrp \ grp`, filtering elements that do not lead to a minimal
    // lexicographic representative at each step in the stabilizer chain.
    val minimal = Array.tabulate(tLength)(k => tInt(k))
    var minimalG = finiteGroupG.id
    def rec(level: Int, toLevel: Int, g: G, chainGrp: Chain[G], chainSym: Grp[G]): Unit = chainGrp match {
      case node: Node[G] if level <= toLevel =>
        implicit def action = node.action
        val candidates = debox.Buffer.empty[Int]
        val orbitIt = node.orbit.iterator
        val beta = node.beta
        val nextBeta = node.next match {
          case nextNode: Node[G] => nextNode.beta
          case _: Term[G] => representation.size
        }
        while (orbitIt.hasNext) {
          val b = orbitIt.next
          val bg = b <|+| g
          var comp = (tInt(bg) - minimal(beta)).signum
          var k = beta + 1
          val u = node.u(b)
          while (k < nextBeta && comp == 0) {
            comp = (tInt((k <|+| u) <|+| g) - tInt(minimal(k))).signum
            k += 1
          }
          if (comp <= 0) {
            val nextG = u |+| g
            if (comp < 0) {
              k = beta
              while (k < tLength) {
                minimal(k) = tInt(k <|+| nextG)
                k += 1
              }
              minimalG = nextG
              candidates.clear
            }
            candidates += b
          }
        }
        var i = 0
        val n = candidates.length
        while (i < n) {
          val b = candidates(i)
          val bg = b <|+| g
          val (nextSym, transversal) = chainSym.stabilizer(bg, representation)
          if (transversal.orbit.min == bg) {
            val nextG = node.u(b) |+| g
            rec(level + 1, toLevel, nextG, node.next, chainSym)
          }
          i += 1
        }
      case _ =>
    }
    var i = 0
    val l = chainInRepresentation.length
    while (i < l) {
      rec(0, i, finiteGroupG.id, chainInRepresentation, symGrp)
      i += 1
    }
    new LexRepresentative[T, G] {
      val element = minimalG.inverse
      val original = t
      implicit val actionTG = self.actionTG
      def rank = BigInt(0)
    }
  }
}
