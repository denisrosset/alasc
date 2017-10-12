package net.alasc.perms

import spire.algebra.{Eq, Group}
import net.alasc.algebra._
import net.alasc.perms.internal.GenPrm
import net.alasc.util._

object PermAlgebra extends Eq[Perm] with Group[Perm] with PermutationAction[Perm] {

  override def toString = "Perm.algebra"

  def isFaithful = true

  def eqv(x: Perm, y: Perm): Boolean = GenPrm.equ.eqv(x.p, y.p)

  def combine(x: Perm, y: Perm): Perm = x |+| y

  def findMovedPoint(p: Perm): NNOption = p.largestMovedPoint

  override def movesAnyPoint(p: Perm): Boolean = !p.isId

  override def movedPoints(p: Perm): Set[Int] = p.movedPoints

  override def nMovedPoints(p: Perm): Int = p.nMovedPoints

  override def smallestMovedPoint(p: Perm): NNOption = p.smallestMovedPoint

  override def largestMovedPoint(p: Perm): NNOption = p.largestMovedPoint

  def actr(preimage: Int, p: Perm): Int = p.image(preimage)

  override def actl(p: Perm, i: Int): Int = p.invImage(i)

  def inverse(p: Perm): Perm = p.inverse

  override def isEmpty(a: Perm)(implicit ev: Eq[Perm]) = a.isId

  def empty = Perm.id

  def movedPointsUpperBound(p: Perm) = new NNOption(p.p.length - 1)

}
