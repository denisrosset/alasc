package net.alasc.algebra

import spire.algebra.{Eq, Action, Group, Semigroup}
import net.alasc.util._


trait PartialAction[P, G] extends Any {
  def partialActl(g: G, p: P): Option[P]
  def partialActr(p: P, g: G): Option[P]
  def isActlDefined(g: G, p: P): Boolean = partialActl(g, p).nonEmpty
  def isActrDefined(p: P, g: G): Boolean = partialActr(p, g).nonEmpty
  def forceActl(g: G, p: P): P = partialActl(g, p) match {
    case Some(result) => result
    case None => throw new IllegalArgumentException(s"Action $g |+|> is not compatible with $p")
  }
  def forceActr(p: P, g: G): P = partialActr(p, g) match {
    case Some(result) => result
    case None => throw new IllegalArgumentException(s"$p is not compatible with action <|+| $g")
  }
  implicit def Forced: Action[P, G] = new Action[P, G] {
    def actl(g: G, p: P): P = forceActl(g, p)
    def actr(p: P, g: G): P = forceActr(p, g)
  }
  implicit def OptionAction: Action[Option[P], G] = new Action[Option[P], G] {
    def actl(g: G, pOpt: Option[P]): Option[P] = pOpt match {
      case Some(p) => partialActl(g, p)
      case None => None
    }
    def actr(pOpt: Option[P], g: G): Option[P] = pOpt match {
      case Some(p) => partialActr(p, g)
      case None => None
    }
  }
}
