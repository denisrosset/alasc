package net.alasc.wreath

import scala.reflect.classTag

import spire.algebra.{Group, PartialOrder}
import spire.algebra.lattice._
import spire.syntax.action._

import metal.syntax._

import net.alasc.algebra._
import net.alasc.finite._
import net.alasc.prep._
import net.alasc.syntax.permutationAction._
import net.alasc.util._

final class WrPrimitivePRepBuilder[A:Group, H:PermutationBuilder](implicit val A: PRepBuilder[A]) extends PRepBuilder[Wr[A, H]] {

  def build(generators: Iterable[Wr[A, H]]) = {
    val n = (1 /: generators) { case (m, g) => m.max(g.aSeq.size).max(g.h.supportMax.getOrElseFast(-1) + 1) }
    val aRep = A.build(generators.flatMap(_.aSeq))
    R(n, aRep)
  }

  case class R(n: Int, aRep: A.R) extends BuiltRep[Wr[A, H]] with FaithfulPRep[Wr[A, H]] {
    type B = WrPrimitivePRepBuilder.this.type
    val builder: B = WrPrimitivePRepBuilder.this
    val aSize = aRep.size
    require(aSize > 1)
    val factors: Array[Int] = Array.fill(n)(aSize).scanLeft(1)(_*_)
    val size = factors.last
    val aDiv = Divisor(size - 1, aSize)
    def represents(w: Wr[A, H]) = w.aSeq.size < n && w.h.supportMax.getOrElseFast(-1) < n && w.aSeq.forall(aRep.represents(_))
    val permutationAction = new FaithfulPermutationAction[Wr[A, H]] {
      def actr(k: Int, w: Wr[A, H]): Int =
        if (k >= size) k else {
          var rem = k
          var i = 0
          var ind = 0
          while (i < n) {
            val nextRem = aDiv.divide(rem)
            val alphai = rem - nextRem * aSize
            rem = nextRem
            ind += factors(i <|+| w.h) * aRep.permutationAction.actr(alphai, w.aSeq.applyOrElse(i, (x: Int) => Group[A].id))
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
            ind += factors(iPrime) * aRep.permutationAction.actl(w.aSeq.applyOrElse(iPrime, (x: Int) => Group[A].id), alphai)
            i += 1
          }
          ind
        }
      def supportMaxElement = size
      def support(w: Wr[A, H]) = {
        val bitset = metal.mutable.BitSet.empty
        var i = 0
        while (i < size) {
          if (actr(i, w) != i)
            bitset += i
          i += 1
        }
        bitset.toScala
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

  val classTagR = classTag[R]

  implicit object partialOrder extends PartialOrder[R] {
    def partialCompare(x: R, y: R) = {
      val sizeC = (x.n - y.n).signum
      val compR = A.partialOrder.partialCompare(x.aRep, y.aRep)
      if (compR == sizeC.toDouble) compR
      Double.NaN
    }
  }

  implicit object lattice extends Lattice[R] with BoundedJoinSemilattice[R] {
    def zero = R(1, A.lattice.zero)
    def join(x: R, y: R) = R(x.n.max(y.n), A.lattice.join(x.aRep, y.aRep))
    def meet(x: R, y: R) = R(x.n.min(y.n), A.lattice.meet(x.aRep, y.aRep))
  }

}
