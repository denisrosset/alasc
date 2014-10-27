package net.alasc
package math
package enum

import scala.collection.mutable

import spire.algebra.{Eq, GroupAction, Order}
import spire.syntax.group._
import spire.syntax.groupAction._
import spire.syntax.cfor._

import net.alasc.algebra._
import net.alasc.syntax.sequence._
import net.alasc.util._

import bsgs._

object Algorithms {
  /** Returns the minimal lexicographic representative of a sequence under permutation.
    * 
    * @param n              Sequence length
    * @param seq            Sequence, defined as a function from index => value
    * @param chainGrp       Group of permutations, described as a BSGS chain with the action corresponding to
    *                       seq, with the base ordered lexicographically (i.e. `node.beta < node.next.beta`)
    * @param symGrp         Subgroup of the group described by `chainGrp` leaving `sym` invariant, i.e.
    *                       for all `h` in `symGrp`, `seq(i <|+| h) = seq(i)`
    * @param representation Representation of `G` corresponding to `seq`
    * 
    * @return the permutation `g` in `chainGrp` such that `i => seq(i <|+| g)` describes a lexicographic minimal
    *         sequence
    */
  def findMinimalPermutation[G: FiniteGroup](n: Int, seq: Int => Int, chainGrp: Chain[G], symGrp: Grp[G], representation: Representation[G]): G = {
    val minimal = new Array[Int](n)
    var minimalCorrectBefore = 0
    var minimalG = FiniteGroup[G].id
    implicit def action = representation.action
    // Implements breadth-first search in the cosets `symGrp \ grp`, filtering elements that do not lead to a minimal
    // lexicographic representative at each step in the stabilizer chain.
    def rec(level: Int, toLevel: Int, curG: G, curChainGrp: Chain[G], curSymGrp: Grp[G]): Unit = curChainGrp match {
      case node: Node[G] if level <= toLevel =>
        val candidates = debox.Buffer.empty[Int]
        val beta = node.beta
        val nextBeta = node.next match {
          case nextNode: Node[G] => nextNode.beta
          case _: Term[G] => n
        }
        if (nextBeta > minimalCorrectBefore) {
          cforRange(minimalCorrectBefore until nextBeta) { k =>
            minimal(k) = seq(k <|+| minimalG)
          }
          minimalCorrectBefore = nextBeta
        }
        node.foreachOrbit { b =>
          val bg = b <|+| curG
          var comp = (seq(bg) - minimal(beta))
          var k = beta + 1
          lazy val nextG = node.u(b) |+| curG
          while (k < nextBeta && comp == 0) {
            comp = (seq(k <|+| nextG) - minimal(k))
            k += 1
          }
          if (comp <= 0) {
            if (comp < 0) {
              cforRange(beta until minimalCorrectBefore) { k =>
                minimal(k) = seq(k <|+| nextG)
              }
              minimalG = nextG
              candidates.clear
            }
            candidates += b
          }
        }
        cforRange(0 until candidates.length) { i => 
          val b = candidates(i)
          val bg = b <|+| curG
          val (nextSymGrp, transversal) = curSymGrp.stabilizer(bg, representation)
          if (transversal.orbitMin == bg) {
            val nextG = node.u(b) |+| curG
            rec(level + 1, toLevel, nextG, node.next, nextSymGrp)
          }
        }
      case _ =>
    }
    cforRange(0 until chainGrp.length) { i =>
      rec(0, i, FiniteGroup[G].id, chainGrp, symGrp)
    }
    minimalG
  }
}
