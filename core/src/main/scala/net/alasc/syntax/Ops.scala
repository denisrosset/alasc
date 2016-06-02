package net.alasc
package syntax

import scala.language.experimental.macros

import spire.macros.Ops

import net.alasc.algebra._
import net.alasc.util._

final class CheckOps[A](lhs: A)(implicit ev: Check[A]) {

  def check(): Check.Checked = ev.check(lhs)

}

final class WidenOps[N](lhs: N) {

  def widen[W](implicit ev: Widen[N, W]): W = macro Ops.unopWithEv[Widen[N, W], W]

}

final class WidenKOps[F[_], N](val lhs: F[N]) extends AnyVal {

  def widenK[W](implicit ev: Widen[F[N], F[W]]): F[W] = ev.widen(lhs)

}

final class PermutationActionOps[A](lhs: A)(implicit ev: PermutationAction[A]) {

  def movesPoint(rhs: Int): Boolean = macro Ops.binop[Int, Boolean]

  def movedPoints(): Set[Int] = macro Ops.unop[Set[Int]]

  def nMovedPoints(): Int = macro Ops.unop[Int]

  def largestMovedPoint(): NNOption = macro Ops.unop[NNOption]

  def smallestMovedPoint(): NNOption = macro Ops.unop[NNOption]

  def findMovedPoint(): NNOption = macro Ops.unop[NNOption]

  def orbit(rhs: Int): Set[Int] = macro Ops.binop[Int, Set[Int]]

  def signPerm(): Int = macro Ops.unop[Int]

  def cycleStructure(): Map[Int, Int] = macro Ops.unop[Map[Int, Int]]

  def images(rhs: Int): IndexedSeq[Int] = macro Ops.binop[Int, IndexedSeq[Int]]

  def toPermutation[Q](implicit evQ: PermutationBuilder[Q]): Q = ev.toPermutation[Q](lhs)

}
