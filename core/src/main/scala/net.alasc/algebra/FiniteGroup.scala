package net.alasc.algebra

import spire.algebra._
import spire.syntax.eq._

/** Type class for finite group elements objects. */
trait FiniteGroup[F] extends Any with Group[F] { self =>
  /** Order of `f`, i.e. the number `k` such that
    * `f |+| f ... k times ... |+| f === id`
    */
  def order(f: F)(implicit eq: Eq[F]): Int = {
    @annotation.tailrec def rec(k: Int, acc: F): Int =
      if (isId(acc)(eq)) k else rec(k + 1, op(f, acc))
    rec(1, f)
  }

  /** Returns `g.inverse |+| f |+| g`. */
  def conjBy(f: F, g: F): F = op(inverse(g), op(f, g))
}

object FiniteGroup {
  def apply[F](implicit ev: FiniteGroup[F]): FiniteGroup[F] = ev
}
