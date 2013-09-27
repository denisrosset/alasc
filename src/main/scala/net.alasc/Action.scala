package net.alasc

import scala.util.Random

trait Action[F <: FiniteElementLike] {
  def faithful: Boolean
  def dimension: Int
  def identity: F
  def domain: Iterator[Dom] = (0 until dimension).toIterator.map(Dom._0(_))
  def apply(f: F, k: Dom): Dom
  def toPerm(f: F): Perm = Perm.fromImages((0 until dimension).map(k => apply(f, Dom._0(k))):_*)
}

case class TrivialAction[E <: PermElement[E]](val identity: E) extends Action[E] {
  def faithful = true
  def dimension = identity.size
  def apply(f: E, k: Dom) = f.image(k)
}
