package net.alasc.wreath

import spire.algebra._
import spire.syntax.action._
import spire.syntax.cfor._

import metal.syntax._

import net.alasc.algebra._
import net.alasc.perms.{FaithfulPermRep, FaithfulPermRepBuilder}
import spire.syntax.group._
import net.alasc.syntax.permutationAction._
import net.alasc.util._

class WrFaithfulPermRepBuilder[A:Eq:Group](implicit val A: FaithfulPermRepBuilder[A]) extends FaithfulPermRepBuilder[Wr[A]] {

  def build[K:Ring](generators: Iterable[Wr[A]]) = {
    val n = (1 /: generators) { case (m, g) => spire.math.max(spire.math.max(m, g.aSeq.size), g.h.largestMovedPoint.getOrElseFast(-1) + 1) }
    val aRep = A.build(generators.flatMap(_.aSeq))
    R(n, aRep)
  }

  case class R[K](n: Int, aRep: FaithfulPermRep[A, K])(implicit val scalar: Ring[K]) extends FaithfulPermRep[Wr[A], K] {
    val aSize = aRep.dimension
    val dimension = n * aSize
    val aDiv = Divisor(dimension - 1, aSize)
    def represents(w: Wr[A]) = {
      var lastNotId = -1
      cforRange(0 until w.aSeq.size) { k => if (!w.aSeq(k).isId) lastNotId = k }
      lastNotId < n && w.h.largestMovedPoint.getOrElseFast(-1) < n && w.aSeq.forall(aRep.represents(_))
    }
    type F = _permutationAction.type
    def permutationAction: F = _permutationAction
    object _permutationAction extends PermutationAction[Wr[A]] {
      def isFaithful = true
      override def movesAnyPoint(w: Wr[A]) = !w.h.isId || w.aSeq.exists(a => !a.isId)
      def findMovedPoint(w: Wr[A]) = largestMovedPoint(w)
      def actr(k: Int, w: Wr[A]): Int =
        if (k >= dimension) k else {
          val block = aDiv.divide(k)
          val sub = k - block * aSize
          val newBlock = block <|+| w.h
          if (block >= w.aSeq.size)
            newBlock * aSize + sub
          else
            newBlock * aSize + aRep.permutationAction.actr(sub, w.aSeq(block))
        }
      def actl(w: Wr[A], k: Int): Int =
        if (k >= dimension) k else {
          val block = aDiv.divide(k)
          val sub = k - block * aSize
          val newBlock = w.h |+|> block
          if (newBlock >= w.aSeq.size)
            newBlock * aSize + sub
          else
            newBlock * aSize + aRep.permutationAction.actl(w.aSeq(newBlock), sub)
        }
      def movedPointsUpperBound(w: Wr[A]) = NNSome(dimension - 1)
      override def movedPoints(w: Wr[A]) = {
        val bitset = metal.mutable.ResizableBitSet.empty
        val m = w.aSeq.size.max(w.h.largestMovedPoint.getOrElseFast(-1) + 1)
        var block = 0
        var offset = 0
        val s = aRep.dimension
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
      override def nMovedPoints(w: Wr[A]) = movedPoints(w).size
      override def smallestMovedPoint(w: Wr[A]): NNOption = {
        var block = 0
        var offset = 0
        val m = w.aSeq.size.max(w.h.largestMovedPoint.getOrElseFast(-1) + 1)
        val s = aRep.dimension
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
      override def largestMovedPoint(w: Wr[A]): NNOption = {
        val m = w.aSeq.size.max(w.h.largestMovedPoint.getOrElseFast(-1) + 1)
        var block = m - 1
        val s = aRep.dimension
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
