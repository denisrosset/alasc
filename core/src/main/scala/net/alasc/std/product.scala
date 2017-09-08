package net.alasc.std

import net.alasc.algebra._
import net.alasc.finite.{FaithfulActionBuilder, FaithfulPermutationActionBuilder}
import net.alasc.util._

final class Product2FaithfulPermutationAction[A, B](val A: PermutationAction[A], dimA: Int, B: PermutationAction[B], dimB: Int) extends PermutationAction[(A, B)] {

  override def toString = s"Product($A, $B)"

  def isFaithful: Boolean = true

  def actr(p: Int, x0: (A, B)) =
    if (p < dimA) A.actr(p, x0._1)
    else if (p < dimA + dimB) B.actr(p - dimA, x0._2) + dimA
    else p

  def actl(x0: (A, B), p: Int) =
    if (p < dimA) A.actl(x0._1, p)
    else if (p < dimA + dimB) B.actl(x0._2, p - dimA) + dimA
    else p

  def findMovedPoint(x0: (A, B)): NNOption =
    A.findMovedPoint(x0._1) match {
      case NNOption(p) => NNOption(p)
      case _ => B.findMovedPoint(x0._2) match {
        case NNOption(p) => NNOption(p + dimA)
        case _ => NNNone
      }
    }
  override def smallestMovedPoint(x0: (A, B)) =
    A.smallestMovedPoint(x0._1) match {
      case NNOption(p) => NNOption(p)
      case _ => B.smallestMovedPoint(x0._2) match {
        case NNOption(p) => NNOption(p + dimA)
        case _ => NNNone
      }
    }

  override def largestMovedPoint(x0: (A, B)) =
    B.largestMovedPoint(x0._2) match {
      case NNOption(p) => NNOption(p + dimA)
      case _ => A.largestMovedPoint(x0._1) match {
        case NNOption(p) => NNOption(p)
        case _ => NNNone
      }
    }
  override def movesAnyPoint(x0: (A, B)) = A.movesAnyPoint(x0._1) || B.movesAnyPoint(x0._2)
  override def movedPoints(x0: (A, B)) =
    A.movedPoints(x0._1) ++ B.movedPoints(x0._2).map(_ + dimA)
  def movedPointsUpperBound(x0: (A, B)) = if (dimA + dimB > 0) NNSome(dimA + dimB) else NNNone
  override def nMovedPoints(x0: (A, B)): Int = A.nMovedPoints(x0._1) + B.nMovedPoints(x0._2)

}

final class Product2FaithfulPermutationActionBuilder[A, B](implicit A: FaithfulPermutationActionBuilder[A], B: FaithfulPermutationActionBuilder[B])
  extends FaithfulActionBuilder[(A, B), Int, PermutationAction[(A, B)]] {

  def apply(generators: Iterable[(A, B)]): PermutationAction[(A, B)] = {
    val generatorsA = generators.map(_._1)
    val generatorsB = generators.map(_._2)
    val fpaA: PermutationAction[A] = A.apply(generatorsA)
    val fpaB: PermutationAction[B] = B.apply(generatorsB)
    val dimA = fpaA.largestMovedPoint(generatorsA).getOrElseFast(-1) + 1
    val dimB = fpaB.largestMovedPoint(generatorsB).getOrElseFast(-1) + 1
    new Product2FaithfulPermutationAction[A, B](fpaA, dimA, fpaB, dimB)
  }

}

trait ProductInstances {

  implicit def product2FaithfulPermutationActionBuilder[A:FaithfulPermutationActionBuilder, B:FaithfulPermutationActionBuilder]: FaithfulPermutationActionBuilder[(A, B)] =
    new Product2FaithfulPermutationActionBuilder[A, B]

}
