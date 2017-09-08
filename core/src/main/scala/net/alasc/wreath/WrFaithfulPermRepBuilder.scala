package net.alasc.wreath

import spire.algebra._
import spire.std.int._
import spire.syntax.action._
import spire.syntax.cfor._
import metal.syntax._

import net.alasc.algebra._

import net.alasc.finite.FaithfulPermutationActionBuilder
import net.alasc.syntax.permutationAction._
import net.alasc.util._

class WrFaithfulPermutationAction[A:Eq:Group](val n: Int, val aSize: Int)(implicit actionA: PermutationAction[A]) extends PermutationAction[Wr[A]] {
  def isFaithful = true
  val dimension = n * aSize
  val aDiv = Divisor(dimension - 1, aSize)
  override def movesAnyPoint(w: Wr[A]) = !w.h.isId || w.aMap.nonEmpty
  def findMovedPoint(w: Wr[A]) = largestMovedPoint(w)
  def actr(k: Int, w: Wr[A]): Int =
    if (k >= dimension) k else {
      val block = aDiv.divide(k)
      val sub = k - block * aSize
      val newBlock = block <|+| w.h
      newBlock * aSize + (sub <|+| w.a(block))
    }
  def actl(w: Wr[A], k: Int): Int =
    if (k >= dimension) k else {
      val block = aDiv.divide(k)
      val sub = k - block * aSize
      val newBlock = w.h |+|> block
      newBlock * aSize + (w.a(newBlock) |+|> sub)
    }
  def movedPointsUpperBound(w: Wr[A]) = NNSome(dimension - 1)
  override def movedPoints(w: Wr[A]) = {
    val bitset = metal.mutable.ResizableBitSet.empty
    val m = w.n
    var block = 0
    var offset = 0
    val s = aSize
    while (block < m) {
      if ((block <|+| w.h) != block) {
        cforRange(offset until (offset + s)) { k =>
          bitset += k
        }
      }
      else w.a(block).movedPoints.foreach { sub => bitset += (offset + sub) }
      block += 1
      offset += s
    }
    bitset.toScala
  }
  override def nMovedPoints(w: Wr[A]) = movedPoints(w).size
  override def smallestMovedPoint(w: Wr[A]): NNOption = {
    var block = 0
    var offset = 0
    val m = w.n
    val s = aSize
    while (block < m) {
      if ((block <|+| w.h) != block)
        return NNSome(offset)
      else w.a(block).smallestMovedPoint match {
          case NNOption(sub) => return NNSome(offset + sub)
          case _ =>
      }
      block += 1
      offset += s
    }
    NNNone
  }
  override def largestMovedPoint(w: Wr[A]): NNOption = {
    val m = n
    var block = m - 1
    val s = aSize
    var offset = block * s
    while (block >= 0) {
      if ((block <|+| w.h) != block)
        return NNSome(offset + s - 1)
      else w.a(block).largestMovedPoint match {
          case NNOption(sub) => return NNSome(offset + sub)
          case _ =>
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
    val aGenerators = generators.flatMap(_.aMap.values)
    val n = generators.mapMax(1)(_.n)
    implicit val actionA = FaithfulPermutationActionBuilder[A].apply(aGenerators)
    val aSize = aGenerators.mapMax(1)(a => a.largestMovedPoint.getOrElseFast(-1) + 1)
    new WrFaithfulPermutationAction[A](n, aSize)
  }

}

object MaxHelpers {

  implicit class TraversableMapMax[A](val coll: Traversable[A]) extends AnyVal {
    def mapMax[B:Order](defaultValue: B)(f: A => B): B =
      coll.foldLeft(defaultValue) { (mx, el) => spire.math.max(mx, f(el)) }
  }

}
