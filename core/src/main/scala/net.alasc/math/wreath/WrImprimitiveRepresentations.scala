package net.alasc.math
package wreath

import scala.language.higherKinds

import scala.collection.generic.CanBuildFrom
import scala.collection.SeqLike
import scala.collection.mutable
import scala.reflect.{ClassTag, classTag}

import spire.algebra._
import spire.algebra.lattice._
import spire.syntax.eq._
import spire.syntax.group._
import spire.syntax.action._

import net.alasc.algebra._
import net.alasc.std.seq._
import net.alasc.syntax.permutationAction._
import net.alasc.syntax.subgroup._
import net.alasc.util._

class WrImprimitiveRepresentations[A, H](implicit val aReps: Representations[A], aAlgebra: FiniteGroup[A], hAlgebra: Permutation[H]) extends Representations[Wr[A, H]] {
  self =>
  type AR = aReps.R
  val RClassTag = classTag[R]
  def get(generators: Iterable[Wr[A, H]]) = {
    val n = (1 /: generators) { case (m, g) => m.max(g.aSeq.size).max(g.h.supportMax.getOrElseFast(-1) + 1) }
    val aRep = aReps.get(generators.flatMap(_.aSeq))
    R(n, aRep)
  }
  implicit object partialOrder extends PartialOrder[R] {
    def partialCompare(x: R, y: R) = {
      val sizeC = (x.n - y.n).signum
      val compR = aReps.partialOrder.partialCompare(x.aRep, y.aRep)
      if (compR == sizeC.toDouble) compR
      Double.NaN
    }
  }
  implicit object lattice extends Lattice[R] with BoundedJoinSemilattice[R] {
    def zero = R(1, aReps.lattice.zero)
    def join(x: R, y: R) = R(x.n.max(y.n), aReps.lattice.join(x.aRep, y.aRep))
    def meet(x: R, y: R) = R(x.n.min(y.n), aReps.lattice.meet(x.aRep, y.aRep))
  }
  case class R(n: Int, aRep: AR) extends Representation[Wr[A, H]] {
    val aSize = aRep.size
    val size = n * aSize
    val aDiv = Divisor(size - 1, aSize)
    val representations = self
    def represents(w: Wr[A, H]) = w.aSeq.size < n && w.h.supportMax.getOrElseFast(-1) < n && w.aSeq.forall(aRep.represents(_))
    val action = new FaithfulPermutationAction[Wr[A, H]] {
      def actr(k: Int, w: Wr[A, H]): Int =
        if (k >= size) k else {
          val block = aDiv.divide(k)
          val sub = k - block * aSize
          val newBlock = block <|+| w.h
          if (block >= w.aSeq.size)
            newBlock * aSize + sub
          else
            newBlock * aSize + aRep.action.actr(sub, w.aSeq(block))
        }
      def actl(w: Wr[A, H], k: Int): Int =
        if (k >= size) k else {
          val block = aDiv.divide(k)
          val sub = k - block * aSize
          val newBlock = w.h |+|> block
          if (newBlock >= w.aSeq.size)
            newBlock * aSize + sub
          else
            newBlock * aSize + aRep.action.actl(w.aSeq(newBlock), sub)
        }
      def supportMaxElement = size - 1
      def support(w: Wr[A, H]) = {
        val bitset = mutable.BitSet.empty
        val m = w.aSeq.size.max(w.h.supportMax.getOrElseFast(-1) + 1)
        var block = 0
        var offset = 0
        val s = aRep.size
        while (block < m) {
          if ((block <|+| w.h) != block)
            bitset ++= offset until (offset + s)
          else if (block < w.aSeq.size)
            aRep.action.support(w.aSeq(block)).foreach { sub => bitset += (offset + sub) }
          block += 1
          offset += s
        }
        bitset.toImmutable
      }
      def supportMin(w: Wr[A, H]): NNOption = {
        var block = 0
        var offset = 0
        val m = w.aSeq.size.max(w.h.supportMax.getOrElseFast(-1) + 1)
        val s = aRep.size
        while (block < m) {
          if ((block <|+| w.h) != block)
            return NNSome(offset)
          else if (block < w.aSeq.size) {
            aRep.action.supportMin(w.aSeq(block)) match {
              case NNOption(sub) => return NNSome(offset + sub)
              case _ =>
            }
          }
          block += 1
          offset += s
        }
        NNNone
      }
      def supportMax(w: Wr[A, H]): NNOption = {
        val m = w.aSeq.size.max(w.h.supportMax.getOrElseFast(-1) + 1)
        var block = m - 1
        val s = aRep.size
        var offset = block * s
        while (block >= 0) {
          if ((block <|+| w.h) != block)
            return NNSome(offset + s - 1)
          else if (block < w.aSeq.size) {
            aRep.action.supportMax(w.aSeq(block)) match {
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
