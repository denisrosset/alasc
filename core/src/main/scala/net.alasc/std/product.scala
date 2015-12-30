package net.alasc.std

import scala.language.higherKinds

import scala.annotation.tailrec

import scala.collection.generic.CanBuildFrom
import scala.collection.SeqLike
import scala.collection.mutable
import scala.reflect.classTag

import spire.algebra._
import spire.algebra.lattice.{Lattice, BoundedJoinSemilattice}
import spire.syntax.eq._
import spire.syntax.group._
import spire.syntax.action._
import spire.util.Opt

import net.alasc.algebra._
import net.alasc.util._

final private[alasc] class RepresentationsProduct2[A, B](implicit val structure1: Representations[A], val structure2: Representations[B]) extends Representations[(A, B)] {

  def get(generators: Iterable[(A, B)]): R =
    R(structure1.get(generators.map(_._1)), structure2.get(generators.map(_._2)))

  val RClassTag = classTag[R]

  case class R(_1: structure1.R, _2: structure2.R) extends Representation[(A, B)] {
    val representations = Opt(RepresentationsProduct2.this)
    def size = _1.size + _2.size
    def represents(x0: (A, B)) = _1.represents(x0._1) && _2.represents(x0._2)
    val action = new FaithfulPermutationAction[(A, B)] {
      @inline def action1 = _1.action
      @inline def action2 = _2.action
      def actr(p: Int, x0: (A, B)) =
        if (p < _1.size) action1.actr(p, x0._1)
        else if (p < _1.size + _2.size) action2.actr(p - _1.size, x0._2) + _1.size
        else p
      def actl(x0: (A, B), p: Int) =
        if (p < _1.size) action1.actl(x0._1, p)
        else if (p < _1.size + _2.size) action2.actl(x0._2, p - _1.size) + _1.size
        else p
      def support(x0: (A, B)) =
        action1.support(x0._1) ++ action2.support(x0._2).map(_ + _1.size)
      def supportMin(x0: (A, B)) =
        action1.supportMin(x0._1).orElseInt(action2.supportMin(x0._2).mapInt(_ + _1.size))
      def supportMax(x0: (A, B)) =
        action2.supportMax(x0._2).mapInt(_ + _1.size).orElseInt(action1.supportMax(x0._1))
      def supportMaxElement = _1.size + _2.size
    }
  }

  object lattice extends Lattice[R] with BoundedJoinSemilattice[R] {
    def zero = R(structure1.lattice.zero, structure2.lattice.zero)
    def join(x0: R, x1: R): R =
      R(structure1.lattice.join(x0._1, x1._1), structure2.lattice.join(x0._2, x1._2))
    def meet(x0: R, x1: R): R =
      R(structure1.lattice.meet(x0._1, x1._1), structure2.lattice.meet(x0._2, x1._2))
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

trait RepresentationsProductInstances {
  implicit def RepresentationsProduct2[A, B](implicit _structure1: Representations[A], _structure2: Representations[B]): Representations[(A, B)] =
    new RepresentationsProduct2[A, B]
}

trait ProductInstances extends
    RepresentationsProductInstances
