package net.alasc.algebra

import spire.NoImplicit
import spire.algebra.{CRing, EuclideanRing, Field, Ring}
import spire.math.{Rational, SafeLong}
import spire.syntax.field._

import scala.annotation.tailrec

trait RingType[A[X] <: Ring[X], B[X] <: Ring[X], R[X] <: Ring[X]]

abstract class RingType0 {
  implicit def ring[A[X] <: Ring[X], B[X] <: Ring[X]]: RingType[A, B, Ring] = null
}

abstract class RingType1 extends RingType0 {
  implicit def cRing[A[X] <: CRing[X], B[X] <: CRing[X]]: RingType[A, B, CRing] = null
}

abstract class RingType2 extends RingType1 {
  implicit def euclideanRing[A[X] <: EuclideanRing[X], B[X] <: EuclideanRing[X]]: RingType[A, B, EuclideanRing] = null
}

object RingType extends RingType2 {
  implicit def field[A[X] <: Field[X], B[X] <: Field[X]]: RingType[A, B, Field] = null
}

trait Homomorphism[A, B, F[_]] extends Function1[A, B] {
  def apply(a: A): B
}

abstract class Homomorphism0 {

//  implicit def intRing[A](implicit A: Ring[A], ev: NoImplicit[A =:= Int]): Homomorphism[Int, A, Ring] = new Homomorphism[Int, A, Ring] {
//    def widen(n: Int) = A.fromInt(n)
//  }


}

object Homomorphism extends Homomorphism0 {

  val maxIntSafeLong = SafeLong(Int.MaxValue)

  /** Homomorphism of a [[spire.math.SafeLong]] to any [[spire.algebra.Ring]] element, using repeated
    * calls to `fromInt`.
    *
    * @param  ev Ring typeclass
    * @tparam B  Destination element type
    *         BR Destination ring type
    *         R  Common ring type
    */
  implicit def SafeLongRing[B, BR[X] <: Ring[X], R[X] <: Ring[X]](implicit ev: BR[B], ev1: RingType[EuclideanRing, BR, R]): Homomorphism[SafeLong, B, R] =
    new Homomorphism[SafeLong, B, R] {

      // TODO: use fromBigInt
      def apply(s: SafeLong): B =
        if (s.isValidInt) ev.fromInt(s.toInt) else {
          val maxIntB = ev.fromInt(Int.MaxValue)

          /** Returns a * n + b. */
          @tailrec def rec(a: B, n: SafeLong, b: B): B =
          if (n.isValidInt) a * ev.fromInt(n.toInt) + b
          else {
            // we compute n = quot * maxInt + mod
            val quot = n / maxIntSafeLong
            val mod = n - quot * maxIntSafeLong
            require(mod.isValidInt)
            // we return a * (quot * maxInt + mod) + b
            //         = (a * maxInt) * quot + (a * mod + b)
            val newA = a * maxIntB
            val newN = quot
            val newB = a * ev.fromInt(mod.toInt) + b
            rec(newA, newN, newB)
          }

          val firstA = maxIntB
          val firstN = s / maxIntSafeLong
          val firstMod = s - firstN * maxIntSafeLong
          require(firstMod.isValidInt)
          val firstB = ev.fromInt(firstMod.toInt)
          rec(firstA, firstN, firstB)
        }
    }

  /** Widening of a [[spire.math.Rational]] to any [[spire.algebra.Field]] element, using the
    * [[spire.math.SafeLong]] conversion defined above, and division of numerator by denominator.
    *
    * @param ev Field typeclass
    * @tparam B Destination field type
    */
  implicit def RationalField[B, BF[X] <: Field[X], R[X] <: Field[X]](implicit ev: BF[B], ev1: RingType[Field, BF, R]): Homomorphism[Rational, B, R] =
    new Homomorphism[Rational, B, R] {

      def apply(r: Rational) = {
        val safeLongRing = SafeLongRing[B, BF, EuclideanRing]
        if (r.denominator.isOne) safeLongRing(r.numerator)
        else {
          val n = safeLongRing(r.numerator)
          val d = safeLongRing(r.denominator)
          n / d
        }
      }

    }

}

trait Monomorphism[A, B, F[_]] extends Homomorphism[A, B, F]
