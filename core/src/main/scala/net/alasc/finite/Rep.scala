package net.alasc.finite

import spire.algebra.{Group, Eq}

import shapeless.Witness

import net.alasc.algebra.Permutation
import net.alasc.perms.FaithfulPermRep
import net.alasc.util.NNOption

trait Rep[G] { self =>

  /** Tests whether this representation can represent the element `g`. */
  def represents(g: G): Boolean

  type Wrap = Rep.Wrap[G, self.type]

  /** Wraps the given group element, to be represented by this representation. */
  def Wrap(g: G): Wrap = {
    require(represents(g))
    new Rep.Wrap[G, self.type](g)
  }

  def wrap(grp: Grp[G])(implicit builder: GrpBuilder[Wrap]): Grp[Wrap] =
    builder.fromGeneratorsAndOrder(grp.generators.map(Wrap(_)), grp.order)

}

object Rep {

  /** Wraps a group element of type G such that it is represented by representation `R`. */
  final class Wrap[G, R <: Rep[G] with Singleton](val underlying: G) extends AnyVal {

    override def toString = s"Wrap($underlying)"

  }

  abstract class Wrap0 {

    implicit def wrapEq[G, R <: Rep[G] with Singleton](implicit G: Eq[G]): Eq[Wrap[G, R]] =
      new Eq[Wrap[G, R]] {

        override def eqv(x: Wrap[G, R], y: Wrap[G, R]): Boolean = G.eqv(x.underlying, y.underlying)

      }

    implicit def wrapGroup[G, R <: Rep[G] with Singleton](implicit G: Group[G]): Group[Wrap[G, R]] =
      new Group[Wrap[G, R]] {

        override def inverse(lhs: Wrap[G, R]): Wrap[G, R] = new Wrap[G, R](G.inverse(lhs.underlying))

        override def id: Wrap[G, R] = new Wrap[G, R](G.id)

        override def op(x: Wrap[G, R], y: Wrap[G, R]): Wrap[G, R] =
          new Wrap[G, R](G.op(x.underlying, y.underlying))

      }

  }

  object Wrap extends Wrap0 {

    case class Predicate[G, R <: FaithfulPermRep[G] with Singleton](p: G => Boolean)
      extends Function1[Wrap[G, R], Boolean] {

      def apply(r: Wrap[G, R]): Boolean = p(r.underlying)

    }

    implicit def repedPermutation[G, R <: FaithfulPermRep[G] with Singleton]
    (implicit equ: Eq[G], group: Group[G], w: Witness.Aux[R]): Permutation[Wrap[G, R]] =
      new Permutation[Wrap[G, R]] {

        val rep: R = w.value

        override def eqv(x: Wrap[G, R], y: Wrap[G, R]): Boolean = equ.eqv(x.underlying, y.underlying)

        override def actl(g: Wrap[G, R], p: Int): Int = rep.permutationAction.actl(g.underlying, p)

        override def actr(p: Int, g: Wrap[G, R]): Int = rep.permutationAction.actr(p, g.underlying)

        override def inverse(a: Wrap[G, R]): Wrap[G, R] =
          new Wrap[G, R](group.inverse(a.underlying))

        override def id: Wrap[G, R] = new Wrap[G, R](group.id)

        override def op(x: Wrap[G, R], y: Wrap[G, R]): Wrap[G, R] =
          new Wrap[G, R](group.op(x.underlying, y.underlying))

        override def smallestMovedPoint(g: Wrap[G, R]): NNOption = rep.permutationAction.smallestMovedPoint(g.underlying)

        override def movedPoints(g: Wrap[G, R]): Set[Int] = rep.permutationAction.movedPoints(g.underlying)

        def nMovedPoints(g: Wrap[G, R]): Int = rep.permutationAction.nMovedPoints(g.underlying)

        override def largestMovedPoint(g: Wrap[G, R]): NNOption = rep.permutationAction.largestMovedPoint(g.underlying)

        override def movedPointsUpperBound: Int = rep.size - 1

      }

  }

}
