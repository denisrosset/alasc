package net.alasc.wreath

import scala.reflect.classTag

import spire.algebra._
import spire.algebra.lattice._
import spire.syntax.action._
import spire.syntax.cfor._

import metal.syntax._

import net.alasc.algebra._
import net.alasc.perms.{FaithfulPermRep, FaithfulPermRepBuilder}
import spire.syntax.group._
import net.alasc.syntax.permutationAction._
import net.alasc.util._

class WrFaithfulPermRepBuilder[A:Eq:Group, H:Permutation](implicit val A: FaithfulPermRepBuilder[A]) extends FaithfulPermRepBuilder[Wr[A, H]] {

  def build(generators: Iterable[Wr[A, H]]) = {
    val n = (1 /: generators) { case (m, g) => spire.math.max(spire.math.max(m, g.aSeq.size), g.h.largestMovedPoint.getOrElseFast(-1) + 1) }
    val aRep = A.build(generators.flatMap(_.aSeq))
    R(n, aRep)
  }

  case class R(n: Int, aRep: FaithfulPermRep[A]) extends FaithfulPermRep[Wr[A, H]] {
    val aSize = aRep.size
    val size = n * aSize
    val aDiv = Divisor(size - 1, aSize)
    def represents(w: Wr[A, H]) = {
      var lastNotId = -1
      cforRange(0 until w.aSeq.size) { k => if (!w.aSeq(k).isId) lastNotId = k }
      lastNotId < n && w.h.largestMovedPoint.getOrElseFast(-1) < n && w.aSeq.forall(aRep.represents(_))
    }
    val permutationAction = new FaithfulPermutationAction[Wr[A, H]] {
      def actr(k: Int, w: Wr[A, H]): Int =
        if (k >= size) k else {
          val block = aDiv.divide(k)
          val sub = k - block * aSize
          val newBlock = block <|+| w.h
          if (block >= w.aSeq.size)
            newBlock * aSize + sub
          else
            newBlock * aSize + aRep.permutationAction.actr(sub, w.aSeq(block))
        }
      def actl(w: Wr[A, H], k: Int): Int =
        if (k >= size) k else {
          val block = aDiv.divide(k)
          val sub = k - block * aSize
          val newBlock = w.h |+|> block
          if (newBlock >= w.aSeq.size)
            newBlock * aSize + sub
          else
            newBlock * aSize + aRep.permutationAction.actl(w.aSeq(newBlock), sub)
        }
      def movedPointsUpperBound = size - 1
      def movedPoints(w: Wr[A, H]) = {
        val bitset = metal.mutable.BitSet.empty
        val m = w.aSeq.size.max(w.h.largestMovedPoint.getOrElseFast(-1) + 1)
        var block = 0
        var offset = 0
        val s = aRep.size
        while (block < m) {
          if ((block <|+| w.h) != block) {
            cforRange(offset until (offset + s)) { k =>
              bitset += k
            }
          }
          else if (block < w.aSeq.size)
            aRep.permutationAction.movedPoints(w.aSeq(block)).foreach { sub => bitset += (offset + sub) }
          block += 1
          offset += s
        }
        bitset.toScala
      }
      def nMovedPoints(w: Wr[A, H]) = movedPoints(w).size
      def smallestMovedPoint(w: Wr[A, H]): NNOption = {
        var block = 0
        var offset = 0
        val m = w.aSeq.size.max(w.h.largestMovedPoint.getOrElseFast(-1) + 1)
        val s = aRep.size
        while (block < m) {
          if ((block <|+| w.h) != block)
            return NNSome(offset)
          else if (block < w.aSeq.size) {
            aRep.permutationAction.smallestMovedPoint(w.aSeq(block)) match {
              case NNOption(sub) => return NNSome(offset + sub)
              case _ =>
            }
          }
          block += 1
          offset += s
        }
        NNNone
      }
      def largestMovedPoint(w: Wr[A, H]): NNOption = {
        val m = w.aSeq.size.max(w.h.largestMovedPoint.getOrElseFast(-1) + 1)
        var block = m - 1
        val s = aRep.size
        var offset = block * s
        while (block >= 0) {
          if ((block <|+| w.h) != block)
            return NNSome(offset + s - 1)
          else if (block < w.aSeq.size) {
            aRep.permutationAction.largestMovedPoint(w.aSeq(block)) match {
              case NNOption(sub) => return NNSome(offset + sub)
              case _ =>
            }
          }
          block -= 1
          offset -= s
        }
        NNNone
      }
    }
  }

}
