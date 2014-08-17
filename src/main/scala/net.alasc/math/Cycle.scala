package net.alasc
package math

import scala.language.implicitConversions

import scala.collection.immutable.BitSet

import spire.algebra._
import spire.syntax.order._
import spire.syntax.groupAction._
import spire.syntax.signed._

import net.alasc.algebra.PermutationAction
import net.alasc.util._

/** Represent a cyclic permutation of non-negative indices. */
class Cycle private[alasc](val seq: Seq[Int]) {
  require(seq.size > 1)

  def toCycles = new Cycles(Seq(this))

  def length = seq.length

  override def toString: String = seq.mkString("(", ",", ")")

  def toStringUsing(symbols: Int => String) =
    seq.map(symbols(_)).mkString("(", ",", ")")

  override def equals(any: Any) = any match {
    // TODO: canEqual
    case that: Cycle => 
      implicit def order = implicitly[Order[Cycle]]
      this === that
     // case that: Perm => TODO
    case _ => false
  }

  // TODO override def hashCode: Int

  def +(n: Int): Cycle =
    if (n < 0) (this - (-n)) else {
      assert(seq.max <= Int.MaxValue - n)
      new Cycle(seq.map(_ + n))
    }

  def -(n: Int): Cycle =
    if (n < 0) (this + (-n)) else {
      assert(seq.min >= n)
      new Cycle(seq.map(_ - n))
    }

  def support: BitSet = BitSet(seq: _*)

  def inverse = Cycle(seq.reverse: _*)
}


class CycleSigned extends Signed[Cycle] {
  override def signum(c: Cycle) = if (c.length % 2 == 0) 1 else -1
}

class CyclePermutationAction extends PermutationAction[Cycle] {
  def support(c: Cycle): BitSet = BitSet(c.seq: _*)
  def supportMax(c: Cycle) = NNSome(c.seq.max)
  def supportMin(c: Cycle) = NNSome(c.seq.min)
  override def supportAny(c: Cycle) = NNSome(c.seq.head)
  def supportMaxElement = Int.MaxValue
  def actl(c: Cycle, k: Int) = c.seq.indexOf(k) match {
    case -1 => k
    case i => c.seq((c.seq.size + i - 1) % c.seq.size)
  }
  def actr(k: Int, c: Cycle) = c.seq.indexOf(k) match {
    case -1 => k
    case i => c.seq((i + 1) % c.seq.size)
  }
}

/** Canonical order for cycles.
  * 
  * Cycles are first sorted by length, and then by first elements.
  */
class CycleOrder extends Order[Cycle] {
  import spire.std.int.IntAlgebra
  implicit val seqOrder: Order[Seq[Int]] = spire.std.seq.SeqOrder[Int, Seq]
  def compare(x: Cycle, y: Cycle): Int =
    if (x.length != y.length)
      x.length - y.length
    else
      x.seq.compare(y.seq)
}

object Cycle {
  implicit final val CyclePermutationAction: PermutationAction[Cycle] = new CyclePermutationAction
  implicit final val CycleSigned: Signed[Cycle] = new CycleSigned
  implicit final val CycleOrder: Order[Cycle] = new CycleOrder

  def id = new Cycle(Seq.empty[Int])
  def apply(seq: Int*): Cycle = seq match {
    case Seq() => id
    case _ => 
      val i = seq.indexOf(seq.min)
      new Cycle(seq.drop(i) ++ seq.take(i))
  }
  def orbit(k: Int, image: Int => Int): Cycle = {
    @scala.annotation.tailrec def rec(cycle: List[Int]): List[Int] = {
      val i = image(cycle.head)
      if (i == k) cycle.reverse else rec(i :: cycle)
    }
    Cycle(rec(k :: Nil): _*)
  }
}
