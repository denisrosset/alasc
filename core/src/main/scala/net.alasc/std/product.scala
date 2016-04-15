package net.alasc.std

import scala.reflect.classTag

import spire.algebra._
import spire.algebra.lattice.{Lattice, BoundedJoinSemilattice}

import net.alasc.algebra._
import net.alasc.finite._
import net.alasc.prep._

final class PRepBuilderProduct2[A, B](val structure1: PRepBuilder[A], val structure2: PRepBuilder[B]) extends PRepBuilder[(A, B)] {

  type Prod = (A, B)

  type R = PRepProduct2

  def classTagR = classTag[PRepProduct2]

  case class PRepProduct2(_1: structure1.R, _2: structure2.R) extends BuiltRep[Prod] with FaithfulPRep[Prod] {
    type B = PRepBuilderProduct2.this.type
    val builder: B = PRepBuilderProduct2.this
    def size = _1.size + _2.size
    def represents(x0: Prod) = _1.represents(x0._1) && _2.represents(x0._2)
    val permutationAction = new FaithfulPermutationAction[Prod] {
      @inline def action1 = _1.permutationAction
      @inline def action2 = _2.permutationAction
      def actr(p: Int, x0: Prod) =
        if (p < _1.size) action1.actr(p, x0._1)
        else if (p < _1.size + _2.size) action2.actr(p - _1.size, x0._2) + _1.size
        else p
      def actl(x0: Prod, p: Int) =
        if (p < _1.size) action1.actl(x0._1, p)
        else if (p < _1.size + _2.size) action2.actl(x0._2, p - _1.size) + _1.size
        else p
      def support(x0: Prod) =
        action1.support(x0._1) ++ action2.support(x0._2).map(_ + _1.size)
      def supportMin(x0: Prod) =
        action1.supportMin(x0._1).orElseInt(action2.supportMin(x0._2).mapInt(_ + _1.size))
      def supportMax(x0: Prod) =
        action2.supportMax(x0._2).mapInt(_ + _1.size).orElseInt(action1.supportMax(x0._1))
      def supportMaxElement = _1.size + _2.size
    }
  }

  def build(generators: Iterable[Prod]): R =
    PRepProduct2(structure1.build(generators.map(_._1)), structure2.build(generators.map(_._2)))

  object lattice extends Lattice[R] with BoundedJoinSemilattice[R] {
    def zero = PRepProduct2(structure1.lattice.zero, structure2.lattice.zero)
    def join(x0: R, x1: R): R =
      PRepProduct2(structure1.lattice.join(x0._1, x1._1), structure2.lattice.join(x0._2, x1._2))
    def meet(x0: R, x1: R): R =
      PRepProduct2(structure1.lattice.meet(x0._1, x1._1), structure2.lattice.meet(x0._2, x1._2))
  }

  object partialOrder extends PartialOrder[R] {
    def partialCompare(x0: R, x1: R): Double = {
      val cA = structure1.partialOrder.partialCompare(x0._1, x1._1)
      val cB = structure2.partialOrder.partialCompare(x0._2, x1._2)
      if (cA < 0 && cB < 0) -1.0
      else if (cA > 0 && cB > 0) 1.0
      else if (cA == 0 && cB == 0) 0.0
      else Double.NaN
    }
  }

}

trait PRepBuilderProductInstances {
  implicit def PRepBuilderProduct2[A, B](implicit _structure1: PRepBuilder[A], _structure2: PRepBuilder[B]): PRepBuilder[(A, B)] =
    new PRepBuilderProduct2[A, B](_structure1, _structure2)
}

trait ProductInstances extends
    PRepBuilderProductInstances

