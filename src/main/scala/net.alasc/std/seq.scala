package net.alasc.std

import scala.language.higherKinds

import scala.annotation.tailrec

import scala.collection.generic.CanBuildFrom
import scala.collection.SeqLike
import scala.collection.mutable
import scala.reflect.classTag

import spire.algebra._
import spire.syntax.eq._
import spire.syntax.group._
import spire.syntax.groupAction._

import net.alasc.algebra._
import net.alasc.util._

// TODO: rewrite Seq support using iterators instead of linear access
class SeqImprimitiveRepresentations[SG <: SeqLike[G, SG], G](implicit val scalarReps: Representations[G], scalarAlgebra: FiniteGroup[G]) extends Representations[SG] {
  self =>
  type SR = scalarReps.R
  def rClassTag = classTag[R]
  def tryCast(genR: Representation[SG]): RefOption[R] = genR match {
    case r: R if r.representations eq self => RefSome(r)
    case _ => RefNone
  }
  def get(generators: Iterable[SG]) = {
    val n = (1 /: generators) { case (m, g) => m.max(g.size) }
    val scalarRep = scalarReps.get(generators.flatMap(identity))
    R(n, scalarRep)
  }
  implicit object lattice extends BoundedBelowLattice[R] {
    def zero = R(1, scalarReps.lattice.zero)
    def partialCompare(x: R, y: R) = {
      val sizeC = (x.n - y.n).signum
      val compR = scalarReps.lattice.partialCompare(x.scalarRep, y.scalarRep)
      if (compR == sizeC.toDouble) compR
      Double.NaN
    }
    def join(x: R, y: R) = R(x.n.max(y.n), scalarReps.lattice.join(x.scalarRep, y.scalarRep))
    def meet(x: R, y: R) = R(x.n.min(y.n), scalarReps.lattice.meet(x.scalarRep, y.scalarRep))
  }
  case class R(n: Int, scalarRep: SR) extends Representation[SG] {
    val size = n * scalarRep.size
    val representations = self
    def represents(sg: SG) = sg.size < n && sg.forall(scalarRep.represents(_))
    val action = new FaithfulPermutationAction[SG] {
      def actr(k: Int, sg: SG): Int =
        if (k >= sg.size * scalarRep.size) k else {
          val s = scalarRep.size
          val sub = k % s
          val block = k / s
          val offset = k - sub
          offset + scalarRep.action.actr(sub, sg(block))
        }
      def actl(sg: SG, k: Int): Int =
        if (k >= sg.size * scalarRep.size) k else {
          val s = scalarRep.size
          val sub = k % s
          val block = k / s
          val offset = k - sub
          offset + scalarRep.action.actl(sg(block), sub)
        }
      def supportMaxElement = size
      def support(sg: SG) = {
        val bitset = mutable.BitSet.empty
        val m = sg.size
        var i = 0
        var offset = 0
        val s = scalarRep.size
        while (i < m) {
          if (!sg(i).isId)
            scalarRep.action.support(sg(i)).foreach { sub => bitset += (offset + sub) }
          i += 1
          offset += s
        }
        bitset.toImmutable
      }
      def supportMin(sg: SG): NNOption = {
        var i = 0
        var offset = 0
        val m = sg.size
        val s = scalarRep.size
        while (i < m) {
          scalarRep.action.supportMin(sg(i)) match {
            case NNOption(sub) => return NNSome(offset + sub)
            case _ =>
          }
          i += 1
          offset += s
        }
        NNNone
      }
      def supportMax(sg: SG): NNOption = {
        val m = sg.size
        var i = m - 1
        val s = scalarRep.size
        var offset = i * s
        while (i >= 0) {
          scalarRep.action.supportMax(sg(i)) match {
            case NNOption(sub) => return NNSome(offset + sub)
            case _ =>
          }
          i -= 1
          offset -= s
        }
        NNNone
      }
    }
  }
}

class SeqFiniteGroup[SG <: SeqLike[G, SG], G](implicit scalar: FiniteGroup[G], cbf: CanBuildFrom[Nothing, G, SG]) extends FiniteGroup[SG] {
  def id: SG = cbf().result
  def eqv(x: SG, y: SG): Boolean = {
    val xs = x.size
    val ys = y.size
    val s = xs.max(ys)
    var i = 0
    while (i < s){
      if (i >= xs) {
        if (!y(i).isId)
          return false
      } else if (i >= ys) {
        if (!x(i).isId)
          return false
      } else {
        if (x(i) =!= y(i))
          return false
      }
      i += 1
    }
    true
  }
  def inverse(sg: SG): SG = {
    val b = cbf()
    b.sizeHint(sg)
    sg.foreach { g => b += g.inverse }
    b.result
  }
  def op(x: SG, y: SG): SG = {
    val b = cbf()
    b.sizeHint(x.size.max(y.size))
    val xi = x.iterator
    val yi = y.iterator
    var stay = true
    while (stay) {
      if (xi.hasNext) {
        if (yi.hasNext)
          b += xi.next |+| yi.next
        else
          b += xi.next
      } else {
        if (yi.hasNext)
          b += yi.next
        else
          stay = false
      }
    }
    b.result
  }
}

class SeqSequence[SA <: SeqLike[A, SA], A] extends Sequence[SA, A] {
  def length(s: SA) = s.length
  def elemAt(s: SA, i: Int): A = s(i)
  def toIndexedSeq(s: SA): IndexedSeq[A] = s.toIndexedSeq
}

class SeqPermutationAction[SA <: SeqLike[A, SA], A, P: FiniteGroup: FaithfulPermutationAction](
  implicit cbf: CanBuildFrom[Nothing, A, SA]) extends GroupAction[SA, P] {
  import net.alasc.syntax.permutationAction._
  import spire.syntax.group._
  import spire.syntax.groupAction._

  def actl(p: P, s: SA): SA = {
    val b = cbf()
    b.sizeHint(s)
    for (i <- 0 until s.length)
      b += s(i <|+| p)
    b.result
  }

  def actr(s: SA, p: P): SA = actl(p.inverse, s)
}

trait SeqInstances0 {
  implicit def SeqSequence[CC[A] <: SeqLike[A, CC[A]], A]: Sequence[CC[A], A] = new SeqSequence[CC[A], A]
  implicit def SeqPermutationAction[CC[A] <: SeqLike[A, CC[A]], A, P: FiniteGroup: FaithfulPermutationAction](
    implicit cbf: CanBuildFrom[Nothing, A, CC[A]]): GroupAction[CC[A], P] = new SeqPermutationAction[CC[A], A, P]
  implicit def seqFiniteGroup[CC[G] <: SeqLike[G, CC[G]], G](
    implicit scalar: FiniteGroup[G], cbf: CanBuildFrom[Nothing, G, CC[G]]): FiniteGroup[CC[G]] = new SeqFiniteGroup[CC[G], G]
  implicit def seqImprimitiveRepresentations[CC[G] <: SeqLike[G, CC[G]], G](
    implicit scalarReps: Representations[G], scalarAlgebra: FiniteGroup[G]): Representations[CC[G]] = new SeqImprimitiveRepresentations[CC[G], G]
}

trait SeqInstances extends SeqInstances0 {

}
