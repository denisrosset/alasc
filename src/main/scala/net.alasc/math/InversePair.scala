package net.alasc.math

import spire.algebra.{Group, GroupAction, Eq}
import net.alasc.algebra.Permutation
import scala.{ specialized => spec }
import scala.language.implicitConversions
import spire.syntax.group._
import spire.NoImplicit

/*import net.alasc.algebra._
import scala.util.Random
import net.alasc.syntax.subgroup._
import net.alasc.syntax.permutation._
*/

case class InversePair[G](g: G, gInv: G)

trait InversePairInstances0 {
  implicit def InversePairGroup[G](implicit algebra: Group[G], ev: NoImplicit[Permutation[G]]): Group[InversePair[G]] = new InversePairGroup[G]
}

trait InversePairInstances1 extends InversePairInstances0 {
  implicit def InversePairPermutation[P](implicit algebra: Permutation[P]): Permutation[InversePair[P]] = new InversePairPermutation[P]
  implicit def inversePair[G](g: G)(implicit algebra: Group[G]): InversePair[G] = InversePair(g, g.inverse)
}

trait InversePairInstances extends InversePairInstances1

object InversePair extends InversePairInstances {
}

class InversePairGroup[G](implicit val algebra: Group[G]) extends Group[InversePair[G]] {
  type I = InversePair[G]
  def inverse(pair: I) = InversePair(pair.gInv, pair.g)
  def id = InversePair(algebra.id, algebra.id)
  def op(x: I, y: I) = InversePair(x.g |+| y.g, y.gInv |+| x.gInv)
  // TODO  override def isId(ip: I) = algebra.isId(ip.g)
}

class InversePairPermutation[P](implicit override val algebra: Permutation[P]) extends InversePairGroup[P] with Permutation[InversePair[P]] {
  def eqv(x: I, y: I) = algebra.eqv(x.g, y.g)
  override def actl(ip: I, p: Int) = algebra.actr(p, ip.gInv)
  def actr(p: Int, ip: I) = algebra.actr(p, ip.g)
  def support(ip: I) = algebra.support(ip.g)
  def supportMax(ip: I) = algebra.supportMax(ip.g)
  def supportMin(ip: I) = algebra.supportMin(ip.g)
  def supportMaxElement = algebra.supportMaxElement
  def signum(ip: I) = algebra.signum(ip.g)
}
