package net.alasc.algebra

import spire.algebra.GroupAction
import net.alasc.util._

trait Groupoid[G <: AnyRef] extends Any {
  def inverse(a: G): G
  def partialOp(x: G, y: G): RefOption[G]
  def isIdentityArrow(a: G): Boolean
}

trait GroupoidAction[P <: AnyRef, G <: AnyRef] extends Any with GroupAction[P, G] {
  implicit def scalar: Groupoid[G]
  def identityArrow(p: P): G
  override def actl(g: G, p: P): P = partialActl(g, p).getOrElse(throw new IllegalArgumentException(s"Action $g |+|> is not compatible with $p"))
  override def actr(p: P, g: G): P = partialActr(p, g).getOrElse(throw new IllegalArgumentException(s"$p is not compatible with action <|+| $g"))

  def partialActl(g: G, p: P): RefOption[P] = partialActr(p, scalar.inverse(g))
  def partialActr(p: P, g: G): RefOption[P]
}
