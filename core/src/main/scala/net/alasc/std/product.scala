package net.alasc.std

import spire.algebra.Ring

import net.alasc.algebra._
import net.alasc.perms.{FaithfulPermRep, FaithfulPermRepBuilder}
import net.alasc.util._

final case class Product2FaithfulPermRep[A, B, K](A: FaithfulPermRep[A, K], B: FaithfulPermRep[B, K])
                                                 (implicit val scalar: Ring[K]) extends FaithfulPermRep[(A, B), K] {
  def dimension = A.dimension + B.dimension
  def represents(x0: (A, B)) = A.represents(x0._1) && B.represents(x0._2)
  object _permutationAction extends PermutationAction[(A, B)] {

    def isFaithful = true

    @inline def action1 = A.permutationAction
    @inline def action2 = B.permutationAction

    def actr(p: Int, x0: (A, B)) =
      if (p < A.dimension) action1.actr(p, x0._1)
      else if (p < A.dimension + B.dimension) action2.actr(p - A.dimension, x0._2) + A.dimension
      else p
    def actl(x0: (A, B), p: Int) =
      if (p < A.dimension) action1.actl(x0._1, p)
      else if (p < A.dimension + B.dimension) action2.actl(x0._2, p - A.dimension) + A.dimension
      else p
    def movesAnyPoint(x0: (A, B)) = action1.movesAnyPoint(x0._1) || action2.movesAnyPoint(x0._2)
    override def movedPoints(x0: (A, B)) =
      action1.movedPoints(x0._1) ++ action2.movedPoints(x0._2).map(_ + A.dimension)
    override def smallestMovedPoint(x0: (A, B)) =
      action1.smallestMovedPoint(x0._1).orElseInt(action2.smallestMovedPoint(x0._2).mapInt(_ + A.dimension))
    override def largestMovedPoint(x0: (A, B)) =
      action2.largestMovedPoint(x0._2).mapInt(_ + A.dimension).orElseInt(action1.largestMovedPoint(x0._1))
    def movedPointsUpperBound(x0: (A, B)) = NNSome(A.dimension + B.dimension)
    override def nMovedPoints(x0: (A, B)): Int = action1.nMovedPoints(x0._1) + action2.nMovedPoints(x0._2)
  }
  type F = _permutationAction.type
  def permutationAction: F = _permutationAction

}

final class Product2FaithfulPermRepBuilder[A:FaithfulPermRepBuilder, B:FaithfulPermRepBuilder]
  extends FaithfulPermRepBuilder[(A, B)] {

  def build[K:Ring](generators: Iterable[(A, B)]): Product2FaithfulPermRep[A, B, K] =
    Product2FaithfulPermRep(
      implicitly[FaithfulPermRepBuilder[A]].build[K](generators.map(_._1)),
      implicitly[FaithfulPermRepBuilder[B]].build[K](generators.map(_._2))
    )

}

trait ProductInstances {

  implicit def product2FaithfulPermRepBuilder[A:FaithfulPermRepBuilder, B:FaithfulPermRepBuilder]: FaithfulPermRepBuilder[(A, B)] =
    new Product2FaithfulPermRepBuilder[A, B]

}
