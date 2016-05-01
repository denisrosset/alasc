package net.alasc.algebra

import scala.annotation.tailrec

import spire.NoImplicit
import spire.syntax.field._
import spire.algebra.{Field, Ring}
import spire.math.{Rational, SafeLong, SafeLongInstances}
import spire.util.Opt

/** Describes the widening of a number type. Does not extend [[Function1]] because we want to enable
  * [[AnyVal]] implementations.
  */
trait Widen[N, W] extends Any {

  /** Widens the `narrow` value to type `W`. */
  def widen(narrow: N): W

}

/** Widening of a [[spire.math.SafeLong]] to any [[spire.algebra.Ring]] element, using repeated
  * calls to `fromInt`.
  *
  * @param  ev  Ring typeclass
  * @tparam A   Destination ring type
  */
final class SafeLongRing[A](val ev: Ring[A]) extends AnyVal with Widen[SafeLong, A] {
  import Widen.maxIntSafeLong
  implicit def A: Ring[A] = ev
  def widen(s: SafeLong): A =
    if (s.isValidInt) A.fromInt(s.toInt) else {
      val maxIntA = A.fromInt(Int.MaxValue)
      /** Returns a * n + b. */
      @tailrec def rec(a: A, n: SafeLong, b: A): A =
        if (n.isValidInt) a * A.fromInt(n.toInt) + b
        else {
          // we compute n = quot * maxInt + mod
          val quot = n / maxIntSafeLong
          val mod = n - quot * maxIntSafeLong
          require(mod.isValidInt)
          // we return a * (quot * maxInt + mod) + b
          //         = (a * maxInt) * quot + (a * mod + b)
          val newA = a * maxIntA
          val newN = quot
          val newB = a * A.fromInt(mod.toInt) + b
          rec(newA, newN, newB)
        }
      val firstA = maxIntA
      val firstN = s / maxIntSafeLong
      val firstMod = s - firstN * maxIntSafeLong
      require(firstMod.isValidInt)
      val firstB = A.fromInt(firstMod.toInt)
      rec(firstA, firstN, firstB)
    }
}

/** Widening of a [[spire.math.Rational]] to any [[spire.algebra.Field]] element, using the
  * [[spire.math.SafeLong]] conversion defined above, and division of numerator by denominator.
  *
  * @param ev Field typeclass
  * @tparam A Destination field type
  */
final class RationalField[A](val ev: Field[A]) extends AnyVal with Widen[Rational, A] {
  import Widen.safeLongRing
  implicit def A: Field[A] = ev
  def widen(r: Rational) =
    if (r.denominator.isOne) safeLongRing[A].widen(r.numerator)
    else {
      val n = safeLongRing[A].widen(r.numerator)
      val d = safeLongRing[A].widen(r.denominator)
      n / d
    }
}

/** Trivial widening between a type and itself. Provides a fast substitution function. */
trait Same[N, W] extends Any with Widen[N, W] {

  def widen(narrow: N): W

  /** Substitutes a higher-kinded type. */
  def subst[F[_]](p: F[N]): F[W]

}

object Same {

  /** Pattern matcher to recognize that the narrow `N` and the wide `W` types are the same in the conversion. */
  def unapply[N, W](widen: Widen[N, W]): Opt[Same[N, W]] =
    if (widen.isInstanceOf[Same[N, W]]) Opt(widen.asInstanceOf[Same[N, W]]) else Opt.empty[Same[N, W]]

}

final class SameImpl[A](val n: Null) extends AnyVal with Same[A, A] {

  def widen(a: A) = a

  def subst[F[_]](p: F[A]): F[A] = p

}

abstract class LowerPriorityWiden {

  val maxIntSafeLong = SafeLong(Int.MaxValue)

  implicit def intRing[A](implicit A: Ring[A], ev: NoImplicit[A =:= Int]): Widen[Int, A] = new Widen[Int, A] {
    def widen(n: Int) = A.fromInt(n)
  }

  implicit def safeLongRing[A](implicit A: Ring[A]): Widen[SafeLong, A] = new SafeLongRing[A](A)

  implicit def rationalField[A](implicit A: Field[A]): Widen[Rational, A] = new RationalField[A](A)

  import scalin.{Mat, Vec}
  import scalin.algebra.{MatEngine, VecEngine}

  implicit def matWiden[N, W, M[X] <: Mat[X]](implicit sw: Widen[N, W], E: MatEngine[W, _ <: M[W]]): Widen[M[N], M[W]] = new Widen[M[N], M[W]] {
    def widen(mn: M[N]): M[W] = sw match {
      case Same(same) => E.fromMat(same.subst(mn))
      case _ => E.map[N](mn)(n => sw.widen(n))
    }
  }

  /*
  implicit def matWiden[N, W, MW <: Mat[W]](implicit sw: Widen[N, W], MW: MatEngine[W, MW]): Widen[Mat[N], MW] = new Widen[Mat[N], MW] {
    def apply(mn: Mat[N]): MW = sw match {
      case Same(same) => MW.fromMat(same.subst(mn))
      case _ => MW.map[N](mn)(n => sw(n))
    }
  }*/
/*
  implicit def vecWiden[N, W, VW <: Vec[W]](implicit sw: Widen[N, W], VW: VecEngine[W, VW]): Widen[Vec[N], VW] = new Widen[Vec[N], VW] {
    def apply(vn: Vec[N]): VW = sw match {
      case Same(same) => VW.fromVec(same.subst(vn))
      case _ => VW.map[N](vn)(n => sw(n))
    }
  }*/

}

object Widen extends LowerPriorityWiden {

  implicit object intSafeLong extends Widen[Int, SafeLong] {
    def widen(n: Int) = SafeLong(n)
  }

  implicit def same[A]: SameImpl[A] = new SameImpl[A](null)

}
