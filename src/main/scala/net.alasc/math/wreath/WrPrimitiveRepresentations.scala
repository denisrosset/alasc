package net.alasc.math
package wreath

import scala.language.higherKinds

import scala.collection.generic.CanBuildFrom
import scala.collection.SeqLike
import scala.collection.mutable
import scala.reflect.ClassTag

import spire.algebra._
import spire.syntax.eq._
import spire.syntax.group._
import spire.syntax.groupAction._

import net.alasc.algebra._
import net.alasc.std.seq._
import net.alasc.syntax.permutationAction._
import net.alasc.syntax.subgroup._
import net.alasc.util._

class WrPrimitiveRepresentations[A, H](implicit val aReps: Representations[A], aAlgebra: FiniteGroup[A], hAlgebra: Permutation[H]) extends Representations[Wr[A, H]] {
  self =>
  type AR = aReps.R
  def tryCast(genR: Representation[Wr[A, H]]): RefOption[R] = genR match {
    case r: R if r.representations eq self => RefSome(r)
    case _ => RefNone
  }
  def get(generators: Iterable[Wr[A, H]]) = {
    val n = (1 /: generators) { case (m, g) => m.max(g.aSeq.size).max(g.h.supportMax.getOrElse(-1) + 1) }
    val aRep = aReps.get(generators.flatMap(_.aSeq))
    R(n, aRep)
  }
  implicit object lattice extends BoundedBelowLattice[R] {
    def zero = R(1, aReps.lattice.zero)
    def partialCompare(x: R, y: R) = {
      val sizeC = (x.n - y.n).signum
      val compR = aReps.lattice.partialCompare(x.aRep, y.aRep)
      if (compR == sizeC.toDouble) compR
      Double.NaN
    }
    def join(x: R, y: R) = R(x.n.max(y.n), aReps.lattice.join(x.aRep, y.aRep))
    def meet(x: R, y: R) = R(x.n.min(y.n), aReps.lattice.meet(x.aRep, y.aRep))
  }
  case class R(n: Int, aRep: AR) extends Representation[Wr[A, H]] {
    val aSize = aRep.size
    require(aSize > 1)
    val factors: Array[Int] = Array.fill(n)(aSize).scanLeft(1)(_*_)
    val size = factors.last
    val aDiv = Divisor(size - 1, aSize)
    val representations = self
    def represents(w: Wr[A, H]) = w.aSeq.size < n && w.h.supportMax.getOrElse(-1) < n && w.aSeq.forall(aRep.represents(_))
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
            ind += factors(i <|+| w.h) * aRep.action.actr(alphai, w.aSeq.applyOrElse(i, (x: Int) => aAlgebra.id))
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
            ind += factors(iPrime) * aRep.action.actl(w.aSeq.applyOrElse(iPrime, (x: Int) => aAlgebra.id), alphai)
            i += 1
          }
          ind
        }
      def supportMaxElement = size
      // TODO: optimized implementation of support??? stuff
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
