package net.alasc
package syntax

import scala.language.experimental.macros

import spire.algebra.Group
import spire.macros.Ops
import spire.algebra.Eq
import spire.math.SafeLong

import net.alasc.algebra._
import net.alasc.perms.Perm
import net.alasc.util._

final class CheckOps[A](lhs: A)(implicit ev: Check[A]) {

  def check(): Check.Checked = ev.check(lhs)

}

final class PermutationActionOps[A](lhs: A)(implicit ev: PermutationAction[A]) {

  def movesAnyPoint(): Boolean = macro Ops.unop[Boolean]

  def movesPoint(rhs: Int): Boolean = macro Ops.binop[Int, Boolean]

  def nMovedPoints(): Int = macro Ops.unop[Int]

  def movedPoints(): Set[Int] = macro Ops.unop[Set[Int]]

  def largestMovedPoint(): NNOption = macro Ops.unop[NNOption]

  def smallestMovedPoint(): NNOption = macro Ops.unop[NNOption]

  def movedPointsUpperBound(): NNOption = macro Ops.unop[NNOption]

  def findMovedPoint(): NNOption = macro Ops.unop[NNOption]

  def signPerm(): Int = macro Ops.unop[Int]

  def cycleStructure(): Map[Int, Int] = macro Ops.unop[Map[Int, Int]]

  def permutationOrder(): SafeLong = macro Ops.unop[SafeLong]

  def orbit(rhs: Int): Set[Int] = macro Ops.binop[Int, Set[Int]]

  def images(rhs: Int): IndexedSeq[Int] = macro Ops.binop[Int, IndexedSeq[Int]]

  def toPerm: Perm = ev.toPerm(lhs)

}

final class GroupOps[A](lhs: A)(implicit ev: Group[A]) {

  def isId(implicit equ: Eq[A]): Boolean = ev.isEmpty(lhs)

}

final class RichGroupOps[A](lhs: Group[A]) {

  def id: A = lhs.empty

}
