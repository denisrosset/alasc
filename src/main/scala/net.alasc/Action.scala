package net.alasc

import spire.macrosk.Ops
import scala.{ specialized => spec }
import scala.language.experimental.macros
import scala.language.implicitConversions

trait Action[@spec(Int) P, F <: Finite[F]] {
  def actr(p: P, f: F): P
  def permutedBy(p: P, f: F): P = actr(p, f)
}

final class ActionOps[P](lhs: P) {
  def <|+|[F](rhs: F)(implicit ev: Action[P, F]): P =
    macro Ops.binopWithEv[F, Action[P, F], P]
  @deprecated("You should use <*", "")
  def permutedBy[F](rhs: F)(implicit ev: Action[P, F]): P =
    macro Ops.binopWithEv[F, Action[P, F], P]
}
