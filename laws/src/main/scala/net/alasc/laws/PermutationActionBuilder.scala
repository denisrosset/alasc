package net.alasc.laws

import org.scalacheck.{Arbitrary, Gen}

import net.alasc.algebra.PermutationAction
import net.alasc.finite.{FaithfulPermutationActionBuilder, Grp}
import net.alasc.util.{NNNone, NNOption}

/** Creeates a PermutationAction, however not necessarily faithful, used for test purposes. */
trait PermutationActionBuilder[G] extends Function1[Iterable[G], PermutationAction[G]] {
  def apply(grp: Grp[G]): PermutationAction[G] = apply(grp.generators)
}

abstract class PermutationActionBuilder0 {

  import PermutationActionBuilder._

  implicit def fromFaithful[G:FaithfulPermutationActionBuilder]: Arbitrary[PermutationActionBuilder[G]] =
    Arbitrary(Gen.oneOf(
      trivial[G],
      fromFaithfulPermutationActionBuilder[G],
      sign[G](fromFaithfulPermutationActionBuilder[G])
    ))

}

object PermutationActionBuilder extends PermutationActionBuilder0 {

  @inline final def apply[G](implicit ev: PermutationActionBuilder[G]): PermutationActionBuilder[G] = ev

  def trivial[G]: PermutationActionBuilder[G] = new PermutationActionBuilder[G] {
    override def toString = "Trivial"
    def apply(generators: Iterable[G]) = PermutationAction.trivial[G]
  }

  def productSign[A:PermutationActionBuilder,B:PermutationActionBuilder]: PermutationActionBuilder[(A, B)] =
    new PermutationActionBuilder[(A, B)] {
      override def toString = "ProductSign(" + PermutationActionBuilder[A] + ", " + PermutationActionBuilder[B] + ")"
      def apply(generators: Iterable[(A, B)]) = {
        val A = PermutationActionBuilder[A].apply(generators.map(_._1))
        val B = PermutationActionBuilder[B].apply(generators.map(_._2))
        new PermutationAction[(A, B)] {
          override def toString = s"ProductSign($A, $B)"
          def isFaithful: Boolean = false
          def findMovedPoint(g: (A, B)): NNOption =
            if (A.signPerm(g._1) != B.signPerm(g._2)) NNOption(0) else NNNone
          def movedPointsUpperBound(g: (A, B)): NNOption = NNOption(1)
          def actr(p: Int, g: (A, B)): Int =
            if (A.signPerm(g._1) != B.signPerm(g._2) && (p == 0 || p == 1)) 1 - p else p
          def actl(g: (A, B), p: Int): Int =
            if (A.signPerm(g._1) != B.signPerm(g._2) && (p == 0 || p == 1)) 1 - p else p
        }
      }
    }

  def product1[A:PermutationActionBuilder, B]: PermutationActionBuilder[(A, B)] =
    new PermutationActionBuilder[(A, B)] {
      override def toString = "Left(" + PermutationActionBuilder[A] + ")"
      def apply(generators: Iterable[(A, B)]) =
        PermutationAction.contramap(PermutationActionBuilder[A].apply(generators.map(_._1)), "left")( (pair: (A, B)) => pair._1 )
    }

  def product2[A, B:PermutationActionBuilder]: PermutationActionBuilder[(A, B)] =
    new PermutationActionBuilder[(A, B)] {
      override def toString = "Right(" + PermutationActionBuilder[B] + ")"
      def apply(generators: Iterable[(A, B)]) =
        PermutationAction.contramap(PermutationActionBuilder[B].apply(generators.map(_._2)), "right")( (pair: (A, B)) => pair._2 )
    }

  def fromFaithfulPermutationActionBuilder[G](implicit ev: FaithfulPermutationActionBuilder[G]): PermutationActionBuilder[G] =
    new PermutationActionBuilder[G] {
      override def toString = ev.toString
      def apply(generators: Iterable[G]) = ev(generators)
    }

  implicit def productArbitrary[A:FaithfulPermutationActionBuilder, B:FaithfulPermutationActionBuilder](implicit A: Arbitrary[PermutationActionBuilder[A]], B: Arbitrary[PermutationActionBuilder[B]]) =
    Arbitrary(Gen.oneOf(
      trivial[(A, B)],
      A.arbitrary.flatMap( implicit a => B.arbitrary.map( implicit b => productSign[A, B] ) ),
      A.arbitrary.map( implicit a => product1[A, B] ),
      B.arbitrary.map( implicit b => product2[A, B] ),
      Gen.const(fromFaithfulPermutationActionBuilder(net.alasc.std.product.product2FaithfulPermutationActionBuilder[A, B]))
    ))

  def sign[G:PermutationActionBuilder]: PermutationActionBuilder[G] = new PermutationActionBuilder[G] {
    override def toString = "Sign(" + PermutationActionBuilder[G] + ")"
    def apply(generators: Iterable[G]) =
      PermutationAction.sign(PermutationActionBuilder[G].apply(generators))
  }

}
