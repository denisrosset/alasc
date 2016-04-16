package net.alasc.finite

import net.alasc.algebra.Permutation
import net.alasc.prep.{FaithfulPRep}
import net.alasc.util.NNOption
import shapeless.Witness
import spire.algebra.{Eq, Group}

/** Wraps a group element of type G such that it is represented by representation `R`. */
final class Reped[G, R <: Rep[G] with Singleton](val underlying: G) extends AnyVal {

  override def toString = s"Reped($underlying)"

}

abstract class Reped0 {

  implicit def repedEq[G, R <: Rep[G] with Singleton](implicit G: Eq[G]): Eq[Reped[G, R]] =
    new Eq[Reped[G, R]] {

      override def eqv(x: Reped[G, R], y: Reped[G, R]): Boolean = G.eqv(x.underlying, y.underlying)

    }

  implicit def repedGroup[G, R <: Rep[G] with Singleton](implicit G: Group[G]): Group[Reped[G, R]] =
    new Group[Reped[G, R]] {

      override def inverse(lhs: Reped[G, R]): Reped[G, R] = new Reped[G, R](G.inverse(lhs.underlying))

      override def id: Reped[G, R] = new Reped[G, R](G.id)

      override def op(x: Reped[G, R], y: Reped[G, R]): Reped[G, R] =
        new Reped[G, R](G.op(x.underlying, y.underlying))

    }

}

object Reped extends Reped0 {

  case class Predicate[G, R <: FaithfulPRep[G] with Singleton](p: G => Boolean) extends Function1[Reped[G, R], Boolean] {

    def apply(r: Reped[G, R]): Boolean = p(r.underlying)

  }

  implicit def repedPermutation[G, R <: FaithfulPRep[G] with Singleton]
  (implicit equ: Eq[G], group: Group[G], w: Witness.Aux[R]): Permutation[Reped[G, R]] =
    new Permutation[Reped[G, R]] {

      val rep: R = w.value

      override def eqv(x: Reped[G, R], y: Reped[G, R]): Boolean = equ.eqv(x.underlying, y.underlying)

      override def actl(g: Reped[G, R], p: Int): Int = rep.permutationAction.actl(g.underlying, p)

      override def actr(p: Int, g: Reped[G, R]): Int = rep.permutationAction.actr(p, g.underlying)

      override def inverse(a: Reped[G, R]): Reped[G, R] =
        new Reped[G, R](group.inverse(a.underlying))

      override def id: Reped[G, R] = new Reped[G, R](group.id)

      override def op(x: Reped[G, R], y: Reped[G, R]): Reped[G, R] =
        new Reped[G, R](group.op(x.underlying, y.underlying))

      override def smallestMovedPoint(g: Reped[G, R]): NNOption = rep.permutationAction.smallestMovedPoint(g.underlying)

      override def movedPoints(g: Reped[G, R]): Set[Int] = rep.permutationAction.movedPoints(g.underlying)

      def nMovedPoints(g: Reped[G, R]): Int = rep.permutationAction.nMovedPoints(g.underlying)

      override def largestMovedPoint(g: Reped[G, R]): NNOption = rep.permutationAction.largestMovedPoint(g.underlying)

      override def movedPointsUpperBound: Int = rep.size - 1

    }

}
