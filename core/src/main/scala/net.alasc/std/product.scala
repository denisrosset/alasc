package net.alasc.std

import net.alasc.algebra._
import net.alasc.perms.{FaithfulPermRep, FaithfulPermRepBuilder}

final class Product2FaithfulPermRepBuilder[A, B]
  (val structure1: FaithfulPermRepBuilder[A],
   val structure2: FaithfulPermRepBuilder[B]) extends FaithfulPermRepBuilder[(A, B)] {

  case class R(_1: FaithfulPermRep[A], _2: FaithfulPermRep[B]) extends FaithfulPermRep[(A, B)] {
    def size = _1.size + _2.size
    def represents(x0: (A, B)) = _1.represents(x0._1) && _2.represents(x0._2)
    val permutationAction = new FaithfulPermutationAction[(A, B)] {
      @inline def action1 = _1.permutationAction
      @inline def action2 = _2.permutationAction
      def actr(p: Int, x0: (A, B)) =
        if (p < _1.size) action1.actr(p, x0._1)
        else if (p < _1.size + _2.size) action2.actr(p - _1.size, x0._2) + _1.size
        else p
      def actl(x0: (A, B), p: Int) =
        if (p < _1.size) action1.actl(x0._1, p)
        else if (p < _1.size + _2.size) action2.actl(x0._2, p - _1.size) + _1.size
        else p
      def movedPoints(x0: (A, B)) =
        action1.movedPoints(x0._1) ++ action2.movedPoints(x0._2).map(_ + _1.size)
      def smallestMovedPoint(x0: (A, B)) =
        action1.smallestMovedPoint(x0._1).orElseInt(action2.smallestMovedPoint(x0._2).mapInt(_ + _1.size))
      def largestMovedPoint(x0: (A, B)) =
        action2.largestMovedPoint(x0._2).mapInt(_ + _1.size).orElseInt(action1.largestMovedPoint(x0._1))
      def movedPointsUpperBound = _1.size + _2.size
      def nMovedPoints(x0: (A, B)): Int = action1.nMovedPoints(x0._1) + action2.nMovedPoints(x0._2)
    }
  }

  def build(generators: Iterable[(A, B)]): R =
    R(structure1.build(generators.map(_._1)), structure2.build(generators.map(_._2)))

}

trait ProductInstances {

  implicit def product2FaithfulPermRepBuilder[A, B]
    (implicit _structure1: FaithfulPermRepBuilder[A],
     _structure2: FaithfulPermRepBuilder[B]): FaithfulPermRepBuilder[(A, B)] =
    new Product2FaithfulPermRepBuilder[A, B](_structure1, _structure2)

}
