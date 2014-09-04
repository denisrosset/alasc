package net.alasc.math

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

/** Describes the wreath product of two objects. */
case class Wr[A, H](aSeq: Seq[A], h: H)

/** Type classes for wreath products. */
object Wr {
  implicit def wrImprimitiveRepresentations[A: FiniteGroup: Representations, H: Permutation]: Representations[Wr[A, H]] = new WrImprimitiveRepresentations[A, H]
  implicit def wrFiniteGroup[A: FiniteGroup, H: Permutation]: FiniteGroup[Wr[A, H]] = new WrFiniteGroup[A, H]
  def grp[SA, SH, A: Representations, H](n: Int, sa: SA, sh: SH)(
    implicit afg: FiniteGroup[A], hp: Permutation[H], sba: Subgroup[SA, A], sbh: Subgroup[SH, H]): Grp[Wr[A, H]] = {
    val aGenerators = for {
      k <- 0 until n
      a <- sa.generators
    } yield Wr(Seq.tabulate(k + 1)( i => if (i == k) a else afg.id ), hp.id)
    val hGenerators = for {
      h <- sh.generators
    } yield Wr(Seq.empty[A], h)
    val order = sa.order.pow(n) * sh.order
    Grp.fromGeneratorsAndOrder(aGenerators ++ hGenerators, order)
  }
}

// TODO: rewrite Seq support using iterators instead of linear access
class WrImprimitiveRepresentations[A, H](implicit val aReps: Representations[A], aAlgebra: FiniteGroup[A], hAlgebra: Permutation[H]) extends Representations[Wr[A, H]] {
  self =>
  type AR = aReps.R
  def tryCast(genR: Representation[Wr[A, H]]): RefOption[R] = genR match {
    case r: R if r.representations eq self => RefSome(r)
    case _ => RefNone
  }
  def minimal = R(1, aReps.minimal)
  def get(generators: Iterable[Wr[A, H]]) = {
    val n = (1 /: generators) { case (m, g) => m.max(g.aSeq.size).max(g.h.supportMax.getOrElse(-1) + 1) }
    val aRep = aReps.get(generators.flatMap(_.aSeq))
    R(n, aRep)
  }
  implicit object lattice extends Lattice[R] {
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
    val size = n * aRep.size
    def representations = self
    def represents(w: Wr[A, H]) = w.aSeq.size < n && w.h.supportMax.getOrElse(-1) < n && w.aSeq.forall(aRep.represents(_))
    val action = new FaithfulPermutationAction[Wr[A, H]] {
      def actr(k: Int, w: Wr[A, H]): Int =
        if (k >= size) k else {
          val s = aRep.size
          val sub = k % s
          val block = k / s
          val newBlock = block <|+| w.h
          if (block >= w.aSeq.size)
            newBlock * s + sub
          else
            newBlock * s + aRep.action.actr(sub, w.aSeq(block))
        }
      def actl(w: Wr[A, H], k: Int): Int =
        if (k >= size) k else {
          val s = aRep.size
          val sub = k % s
          val block = k / s
          val newBlock = w.h |+|> block
          if (newBlock >= w.aSeq.size)
            newBlock * s + sub
          else
            newBlock * s + aRep.action.actl(w.aSeq(newBlock), sub)
        }
      def supportMaxElement = size
      def support(w: Wr[A, H]) = {
        val bitset = mutable.BitSet.empty
        val m = w.aSeq.size.max(w.h.supportMax.getOrElse(-1) + 1)
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
        val m = w.aSeq.size.max(w.h.supportMax.getOrElse(-1) + 1)
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
        val m = w.aSeq.size.max(w.h.supportMax.getOrElse(-1) + 1)
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

class WrPrimitiveRepresentations[A, H](implicit val aReps: Representations[A], aAlgebra: FiniteGroup[A], hAlgebra: Permutation[H]) extends Representations[Wr[A, H]] {
  self =>
  type AR = aReps.R
  def tryCast(genR: Representation[Wr[A, H]]): RefOption[R] = genR match {
    case r: R if r.representations eq self => RefSome(r)
    case _ => RefNone
  }
  def minimal = R(1, aReps.minimal)
  def get(generators: Iterable[Wr[A, H]]) = {
    val n = (1 /: generators) { case (m, g) => m.max(g.aSeq.size).max(g.h.supportMax.getOrElse(-1) + 1) }
    val aRep = aReps.get(generators.flatMap(_.aSeq))
    R(n, aRep)
  }
  implicit object lattice extends Lattice[R] {
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
    def representations = self
    def represents(w: Wr[A, H]) = w.aSeq.size < n && w.h.supportMax.getOrElse(-1) < n && w.aSeq.forall(aRep.represents(_))
    val action = new FaithfulPermutationAction[Wr[A, H]] {
      def actr(k: Int, w: Wr[A, H]): Int =
        if (k >= size) k else {
          var rem = k
          var i = 0
          var ind = 0
          while (i < n) {
            val alphai = rem % aSize
            rem = rem / aSize
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

class WrFiniteGroup[A, H](implicit aAlgebra: FiniteGroup[A], hAlgebra: Permutation[H]) extends FiniteGroup[Wr[A, H]] {
  def eqv(x: Wr[A, H], y: Wr[A, H]): Boolean = (x.h === y.h) && (x.aSeq === y.aSeq)
  def id = Wr(Seq.empty[A], hAlgebra.id)
  def inverse(w: Wr[A, H]): Wr[A, H] = {
    val hInv = w.h.inverse
    val n = w.aSeq.size.max(w.h.supportMax.getOrElse(-1) + 1)
    Wr(Seq.tabulate(n)( i => w.aSeq.applyOrElse(i <|+| hInv, (k: Int) => aAlgebra.id).inverse), hInv)
  }
  def op(x: Wr[A, H], y: Wr[A, H]): Wr[A, H] = {
    val newH = x.h |+| y.h
    val n = x.aSeq.size.max(y.aSeq.size).max(x.h.supportMax.getOrElse(-1) + 1)
    Wr(Seq.tabulate(n)( i => x.aSeq.applyOrElse(i, (k: Int) => aAlgebra.id) |+| y.aSeq.applyOrElse(i <|+| x.h, (k: Int) => aAlgebra.id) ), newH)
  }
}
