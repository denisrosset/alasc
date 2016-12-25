package net.alasc.wreath

import spire.algebra._
import spire.std.int._
import spire.syntax.action._
import spire.syntax.cfor._
import metal.syntax._

import net.alasc.algebra._
import spire.syntax.group._

import net.alasc.finite.FaithfulPermutationActionBuilder
import net.alasc.syntax.permutationAction._
import net.alasc.util._

class WrFaithfulPermutationAction[A:Eq:Group](val n: Int, val aSize: Int)(implicit actionA: PermutationAction[A]) extends PermutationAction[Wr[A]] {
  def isFaithful = true
  val dimension = n * aSize
  val aDiv = Divisor(dimension - 1, aSize)
  override def movesAnyPoint(w: Wr[A]) = !w.h.isId || w.aSeq.exists(a => !a.isId)
  def findMovedPoint(w: Wr[A]) = largestMovedPoint(w)
  def actr(k: Int, w: Wr[A]): Int =
    if (k >= dimension) k else {
      val block = aDiv.divide(k)
      val sub = k - block * aSize
      val newBlock = block <|+| w.h
      if (block >= w.aSeq.size)
        newBlock * aSize + sub
      else
        newBlock * aSize + (sub <|+| w.aSeq(block))
    }
  def actl(w: Wr[A], k: Int): Int =
    if (k >= dimension) k else {
      val block = aDiv.divide(k)
      val sub = k - block * aSize
      val newBlock = w.h |+|> block
      if (newBlock >= w.aSeq.size)
        newBlock * aSize + sub
      else
        newBlock * aSize + (w.aSeq(newBlock) |+|> sub)
    }
  def movedPointsUpperBound(w: Wr[A]) = NNSome(dimension - 1)
  override def movedPoints(w: Wr[A]) = {
    val bitset = metal.mutable.ResizableBitSet.empty
    val m = w.aSeq.size.max(w.h.largestMovedPoint.getOrElseFast(-1) + 1)
    var block = 0
    var offset = 0
    val s = aSize
    while (block < m) {
      if ((block <|+| w.h) != block) {
        cforRange(offset until (offset + s)) { k =>
          bitset += k
        }
      }
      else if (block < w.aSeq.size)
        w.aSeq(block).movedPoints.foreach { sub => bitset += (offset + sub) }
      block += 1
      offset += s
    }
    bitset.toScala
  }
  override def nMovedPoints(w: Wr[A]) = movedPoints(w).size
  override def smallestMovedPoint(w: Wr[A]): NNOption = {
    var block = 0
    var offset = 0
    val m = w.aSeq.size.max(w.h.largestMovedPoint.getOrElseFast(-1) + 1)
    val s = aSize
    while (block < m) {
      if ((block <|+| w.h) != block)
        return NNSome(offset)
      else if (block < w.aSeq.size) {
        w.aSeq(block).smallestMovedPoint match {
          case NNOption(sub) => return NNSome(offset + sub)
          case _ =>
        }
      }
      block += 1
      offset += s
    }
    NNNone
  }
  override def largestMovedPoint(w: Wr[A]): NNOption = {
    val m = w.aSeq.size.max(w.h.largestMovedPoint.getOrElseFast(-1) + 1)
    var block = m - 1
    val s = aSize
    var offset = block * s
    while (block >= 0) {
      if ((block <|+| w.h) != block)
        return NNSome(offset + s - 1)
      else if (block < w.aSeq.size) {
        w.aSeq(block).largestMovedPoint match {
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

class WrFaithfulPermutationActionBuilder[A:Eq:FaithfulPermutationActionBuilder:Group] extends FaithfulPermutationActionBuilder[Wr[A]] {

  def apply(generators: Iterable[Wr[A]]) = {
    import MaxHelpers._
    val aGenerators = generators.flatMap(_.aSeq)
    val n = generators.mapMax(0)( g => spire.math.max(g.aSeq.size, g.h.largestMovedPoint.getOrElseFast(-1) + 1) )
    implicit val actionA = FaithfulPermutationActionBuilder[A].apply(aGenerators)
    val aSize = aGenerators.mapMax(0)(a => a.largestMovedPoint.getOrElseFast(-1) + 1)
    new WrFaithfulPermutationAction[A](n, aSize)
  }

}

object MaxHelpers {

  implicit class TraversableMapMax[A](val coll: Traversable[A]) extends AnyVal {
    def mapMax[B:Order](defaultValue: B)(f: A => B): B =
      coll.foldLeft(defaultValue) { (mx, el) => spire.math.max(mx, f(el)) }
  }

}
