package net.alasc.math

import scala.language.implicitConversions

import scala.collection.immutable.BitSet
import scala.runtime.RichInt

import spire.algebra._
import spire.syntax.eq._
import spire.syntax.groupAction._
import spire.syntax.signed._

import net.alasc.algebra._
import net.alasc.util._

/** Description of a permutation as a product of disjoint cycles in the canonical form.
  * 
  * Canonical form means:
  * 
  * - each disjoint cycle start with its minimal element,
  * - cycles are sorted in the canonical order, i.e. by length first, and then
  *   by first elements.
  */
class Cycles private[alasc](val seq: Seq[Cycle]) {
  override def toString: String = seq.mkString
  def toStringUsing(symbols: Int => String) =
    seq.map(_.toStringUsing(symbols(_))).mkString
  def apply(cycle: Int*) = Cycles.Algebra.op(this, Cycles(cycle: _*))
  def apply(cycle: String): Cycles = apply(cycle.map(DomainAlphabet.map(_)): _*)
}

class CyclesPermutation extends Permutation[Cycles] {
  implicit val seqEq: Eq[Seq[Cycle]] = spire.std.seq.SeqEq[Cycle, Seq]

  def supportMaxElement = Int.MaxValue

  def fromImages(images: Seq[Int]): Cycles = {
    val support = BitSet(0 until images.size:_*).filter(k => images(k) != k)
    fromSupportAndImageFun(support, images(_))
  }

  def fromSupportAndImageFun(support: Set[Int], image: Int => Int): Cycles = {
    @scala.annotation.tailrec def rec(cycles: List[Cycle], remSupport: Set[Int]): Cycles = 
      remSupport.isEmpty match {
        case true => fromDisjointCycles(cycles)
        case false =>
          val k = remSupport.head
          Cycle.orbit(k, image) match {
            case Some(newCycle) => rec(newCycle :: cycles, remSupport -- newCycle.seq)
            case None => rec(cycles, remSupport - k)
          }
      }
    rec(Nil, support)
  }

  def fromDisjointCycles(cycles: Seq[Cycle]) = {
    import spire.compat._
    new Cycles(cycles.filter(_.length > 1).sorted)
  }

  def eqv(x: Cycles, y: Cycles) = x.seq === y.seq

  def id = new Cycles(Seq.empty[Cycle])

  def op(x: Cycles, y: Cycles) = 
    Cycles.Algebra.fromSupportAndImageFun(support(x) ++ support(y), (i: Int) => actr(actr(i, x), y))

  def inverse(a: Cycles) = Cycles.Algebra.fromDisjointCycles(a.seq.map(_.inverse))

  def actr(k: Int, g: Cycles) = (k /: g.seq) { case (kIt, cycle) => kIt <|+| cycle }
  override def actl(g: Cycles, k: Int) = (k /: g.seq) { case (kIt, cycle) => cycle |+|> kIt }

  override def signum(g: Cycles) = (1 /: g.seq) { case (sIt, cycle) => sIt * cycle.signum }

  def plus(c: Cycles, n: Int) = new Cycles(c.seq.map(_ + n))
  def minus(c: Cycles, n: Int) = new Cycles(c.seq.map(_ - n))

  override def supportAny(c: Cycles) =
    if (c.seq.isEmpty) NNNone else NNSome(c.seq.head.seq.head)

  def supportMin(c: Cycles) = c.seq.flatMap(_.seq).reduceOption(_.min(_)).fold(NNNone)(NNSome(_))
  def supportMax(c: Cycles) = c.seq.flatMap(_.seq).reduceOption(_.max(_)).fold(NNNone)(NNSome(_))
  def support(c: Cycles): BitSet =
    (BitSet.empty /: c.seq) { case (set, cycle) => set union cycle.support }
}

object Cycles {
  implicit val Algebra = new CyclesPermutation
  def apply(seq: Int*): Cycles = seq.size match {
    case 0 | 1 => Algebra.id
    case _ => new Cycles(Seq(Cycle(seq: _*)))
  }
  def apply(cycle: String): Cycles = apply(cycle.map(DomainAlphabet.map(_)): _*)

  implicit def cycleToCycles(c: Cycle): Cycles = new Cycles(Seq(c))
}
