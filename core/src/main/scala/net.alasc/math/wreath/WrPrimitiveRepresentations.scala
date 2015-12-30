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
import spire.util.Opt

import net.alasc.algebra._
import net.alasc.std.seq._
import net.alasc.syntax.permutationAction._
import net.alasc.syntax.subgroup._
import net.alasc.util._

class WrPrimitiveRepresentations[A, H](implicit val aReps: Representations[A], aGroup: Group[A], hAlgebra: Permutation[H]) extends Representations[Wr[A, H]] {
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
    require(aSize > 1)
    val factors: Array[Int] = Array.fill(n)(aSize).scanLeft(1)(_*_)
    val size = factors.last
    val aDiv = Divisor(size - 1, aSize)
    val representations = Opt(self)
    def represents(w: Wr[A, H]) = w.aSeq.size < n && w.h.supportMax.getOrElseFast(-1) < n && w.aSeq.forall(aRep.represents(_))
    val action = new FaithfulPermutationAction[Wr[A, H]] {
      def actr(k: Int, w: Wr[A, H]): Int =
        if (k >= size) k else {
          var rem = k
          var i = 0
          var ind = 0
          while (i < n) {
            val nextRem = aDiv.divide(rem)
            val alphai = rem - nextRem * aSize
            rem = nextRem
            ind += factors(i <|+| w.h) * aRep.action.actr(alphai, w.aSeq.applyOrElse(i, (x: Int) => aGroup.id))
            i += 1
          }
          ind
        }
      def actl(w: Wr[A, H], k: Int): Int =
        if (k >= size) k else {
          var rem = k
          var i = 0
          var ind = 0
          while (i < n) {
            val alphai = rem % aSize
            rem = rem / aSize
            val iPrime = w.h |+|> i
            ind += factors(iPrime) * aRep.action.actl(w.aSeq.applyOrElse(iPrime, (x: Int) => aGroup.id), alphai)
            i += 1
          }
          ind
        }
      def supportMaxElement = size
      def support(w: Wr[A, H]) = {
        val bitset = mutable.BitSet.empty
        var i = 0
        while (i < size) {
          if (actr(i, w) != i)
            bitset += i
          i += 1
        }
        bitset.toImmutable
      }
      def supportMin(w: Wr[A, H]): NNOption = {
        var i = 0
        while (i < size) {
          if (actr(i, w) != i)
            return NNSome(i)
          i += 1
        }
        NNNone
      }
      def supportMax(w: Wr[A, H]): NNOption =  {
        var i = size - 1
        while (i >= 0) {
          if (actr(i, w) != i)
            return NNSome(i)
          i -= 1
        }
        NNNone
      }
    }
  }
}
