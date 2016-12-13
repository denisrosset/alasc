package net.alasc.perms

import scala.language.implicitConversions
import scala.collection.immutable.BitSet

import spire.algebra._
import spire.syntax.eq._
import spire.syntax.action._

import net.alasc.algebra._
import net.alasc.domains.Domain
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

  override def equals(other: Any) = other match {
    case that: Cycles => Cycles.algebra.eqv(this, that)
    case _ => false
  }

  override def hashCode = seq.hashCode

  override def toString: String = "Cycles" + string

  def string: String = seq.map(_.string).mkString

  def stringUsing(symbols: Int => String): String =
    seq.map(_.stringUsing(symbols(_))).mkString

  def apply(cycle: Int*) = Cycles.algebra.op(this, Cycles(cycle: _*))

  def apply(cycle: String): Cycles = apply(cycle.map(Domain.alphabetMap(_)): _*)

}

class CyclesAlgebra extends PermutationAction[Cycles] with Group[Cycles] with Eq[Cycles] {

  implicit val seqEq: Eq[Seq[Cycle]] = spire.std.seq.SeqEq[Cycle, Seq]

  def movesAnyPoint(c: Cycles): Boolean = c.seq.nonEmpty

  def eqv(x: Cycles, y: Cycles) = x.seq === y.seq

  def id = Cycles.id

  def op(x: Cycles, y: Cycles) = 
    Cycles.fromSupportAndImageFun(movedPoints(x) ++ movedPoints(y), (i: Int) => actr(actr(i, x), y))

  def inverse(a: Cycles) = Cycles.fromDisjointCycles(a.seq.map(_.inverse))

  def actr(k: Int, g: Cycles) = (k /: g.seq) { case (kIt, cycle) => kIt <|+| cycle }
  override def actl(g: Cycles, k: Int) = (k /: g.seq) { case (kIt, cycle) => cycle |+|> kIt }

  def movedPointsUpperBound(c: Cycles) = largestMovedPoint(c)
  override def findMovedPoint(c: Cycles) =
    if (c.seq.isEmpty) NNNone else NNSome(c.seq.head.seq.head)
  override def smallestMovedPoint(c: Cycles) = c.seq.flatMap(_.seq).reduceOption(_.min(_)).fold(NNNone)(NNSome(_))
  override def largestMovedPoint(c: Cycles) = c.seq.flatMap(_.seq).reduceOption(_.max(_)).fold(NNNone)(NNSome(_))
  override def movedPoints(c: Cycles): BitSet =
    (BitSet.empty /: c.seq) { case (set, cycle) => set union cycle.support }
  override def nMovedPoints(c: Cycles): Int =
    c.seq.foldLeft(0) { case (acc, c) => acc + Cycle.permutationAction.nMovedPoints(c) }

}

object Cycles {

  val id: Cycles = new Cycles(Seq.empty[Cycle])

  implicit val algebra: PermutationAction[Cycles] with Group[Cycles] with Eq[Cycles] = new CyclesAlgebra

  def apply(seq: Int*): Cycles = seq.size match {
    case 0 | 1 => Cycles.id
    case _ => new Cycles(Seq(Cycle(seq: _*)))
  }

  def apply(cycle: String): Cycles = apply(cycle.map(Domain.alphabetMap(_)): _*)

  implicit def cycleToCycles(c: Cycle): Cycles = new Cycles(Seq(c))

  def fromImages(images: Seq[Int]): Cycles = {
    val support = BitSet(0 until images.size:_*).filter(k => images(k) != k)
    fromSupportAndImageFun(support, images(_))
  }

  def fromImages(images: Array[Int]): Cycles = fromImages(images: Seq[Int])

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

  def fromPerm(p: Perm): Cycles =
    fromSupportAndImageFun(p.movedPoints, k => (k <|+| p))


  def fromDisjointCycles(cycles: Seq[Cycle]) = {
    import spire.compat._
    new Cycles(cycles.filter(_.length > 1).sorted)
  }

}
