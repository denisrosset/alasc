package net.alasc.algebra

import spire.algebra.{Group, GroupAction, Eq}
import scala.{ specialized => spec }
import scala.language.implicitConversions
import spire.syntax.group._
import spire.NoImplicit

case class InversePair[G](g: G, gInv: G)

object InversePair {
  implicit def InversePairGroup[G](implicit algebra: Group[G]): Group[InversePair[G]] = new InversePairGroup[G]
  implicit def InversePairFaithFulPermutationAction[P](implicit action: FaithfulPermutationAction[P]): FaithfulPermutationAction[InversePair[P]] =
    new InversePairFaithfulPermutationAction[P]
  implicit def inversePair[G](g: G)(implicit algebra: Group[G]): InversePair[G] = InversePair(g, g.inverse)
}

class InversePairGroup[G](implicit val algebra: Group[G]) extends Group[InversePair[G]] {
  type I = InversePair[G]
  def inverse(pair: I) = InversePair(pair.gInv, pair.g)
  def id = InversePair(algebra.id, algebra.id)
  def op(x: I, y: I) = InversePair(x.g |+| y.g, y.gInv |+| x.gInv)
}

class InversePairFaithfulPermutationAction[P](implicit val algebra: FaithfulPermutationAction[P]) extends FaithfulPermutationAction[InversePair[P]] {
   type I = InversePair[P]
  def actl(ip: I, p: Int) = algebra.actr(p, ip.gInv)
  def actr(p: Int, ip: I) = algebra.actr(p, ip.g)
  def support(ip: I) = algebra.support(ip.g)
  override def supportAny(ip: I) = algebra.supportAny(ip.g)
  def supportMax(ip: I) = algebra.supportMax(ip.g)
  def supportMin(ip: I) = algebra.supportMin(ip.g)
  def supportMaxElement = algebra.supportMaxElement
  override def signum(ip: I) = algebra.signum(ip.g)
}
