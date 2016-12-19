package net.alasc.perms


import scala.collection.immutable.BitSet

import spire.algebra._
import spire.syntax.order._

import net.alasc.algebra.PermutationAction
import net.alasc.util._

/** Represent a cyclic permutation of non-negative indices.
  * 
  * The cycle elements are presented such that the first
  * element is the minimal one.
  */
class Cycle private[alasc](val seq: Seq[Int]) {
  require(seq.size > 1)

  def toCycles = new Cycles(Seq(this))

  def length = seq.length

  override def toString: String = "Cycle" + string

  def string: String = seq.mkString("(", ",", ")")

  def stringUsing(symbols: Int => String) =
    seq.map(symbols(_)).mkString("(", ",", ")")

  override def equals(any: Any) = any match {
    case that: Cycle => Cycle.order.eqv(this, that)
    case _ => false
  }

  override def hashCode: Int = seq.hashCode

  def support: BitSet = BitSet(seq: _*)

  def inverse = Cycle(seq.reverse: _*)
}

class CyclePermutationAction extends PermutationAction[Cycle] {
  def isFaithful = true
  override def movesAnyPoint(c: Cycle): Boolean = c.seq.nonEmpty
  override def movedPoints(c: Cycle): BitSet = BitSet(c.seq: _*)
  override def largestMovedPoint(c: Cycle) = NNSome(c.seq.max)
  override def smallestMovedPoint(c: Cycle) = NNSome(c.seq.min)
  override def nMovedPoints(c: Cycle) = c.seq.size
  override def findMovedPoint(c: Cycle) = NNSome(c.seq.head)
  def movedPointsUpperBound(c: Cycle) = NNSome(c.seq.max)
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

  implicit final val permutationAction: PermutationAction[Cycle] = new CyclePermutationAction

  implicit final val order: Order[Cycle] = new CycleOrder

  def id = new Cycle(Seq.empty[Int])

  def apply(seq: Int*): Cycle = seq match {
    case Seq() => id
    case _ => 
      val i = seq.indexOf(seq.min)
      new Cycle(seq.drop(i) ++ seq.take(i))
  }

  def orbit(k: Int, image: Int => Int): Option[Cycle] = {
    @scala.annotation.tailrec def rec(cycle: List[Int]): List[Int] = {
      val i = image(cycle.head)
      if (i == k) cycle.reverse else rec(i :: cycle)
    }
    val res = rec(k :: Nil)
    if (res.size == 1) None else Some(Cycle(rec(k :: Nil): _*))
  }

}
