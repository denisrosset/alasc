package net.alasc.algebra

import spire.NoImplicit
import spire.syntax.field._
import spire.algebra.{Field, Ring}
import spire.math.{Rational, SafeLong, SafeLongInstances}
import spire.util.Opt

/*
/** Describes the widening of a number type. Does not extend [[Function1]] because we want to enable
  * [[AnyVal]] implementations.
  */
trait Widen[N, W] extends Any {

  /** Widens the `narrow` value to type `W`. */
  def widen(narrow: N): W

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
*/