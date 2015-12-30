package net.alasc
package syntax

import scala.language.experimental.macros
import scala.reflect.ClassTag
import scala.util.Random

import spire.macros.Ops
import spire.algebra.Monoid

import net.alasc.algebra._
import net.alasc.math.Grp
import net.alasc.util._

final class CheckOps[A](lhs: A)(implicit ev: Check[A]) {
  def check(): Check.Checked = ev.check(lhs)
}

final class PermutationActionOps[A](lhs: A)(implicit ev: PermutationAction[A]) {
  def inSupport(rhs: Int): Boolean = macro Ops.binop[Int, Boolean]
  def support(): Set[Int] = macro Ops.unop[Set[Int]]
  def supportMax(): NNOption = macro Ops.unop[NNOption]
  def supportMin(): NNOption = macro Ops.unop[NNOption]
  def supportAny(): NNOption = macro Ops.unop[NNOption]
  def orbit(rhs: Int): Set[Int] = macro Ops.binop[Int, Set[Int]]
  def images(rhs: Int): IndexedSeq[Int] = macro Ops.binop[Int, IndexedSeq[Int]]
  def to[Q](implicit evQ: Permutation[Q]): Q = ev.to[Q](lhs)
}

final class ShiftablePermutationOps[A](lhs: A)(implicit ev: ShiftablePermutation[A]) {
  def +(rhs: Int): A = macro Ops.binop[Int, A]
  def -(rhs: Int): A = macro Ops.binop[Int, A]
}

final class WithBaseSemigroupoidOps[G, B](lhs: G)(implicit ev: WithBase[G, B]) {
  def source(): B = macro Ops.unop[B]
  def target(): B = macro Ops.unop[B]
}

final class PartialMonoidWithBaseOps[G, B](lhs: B)(implicit ev: PartialMonoidWithBase[G, B]) {
  def id(): G = macro Ops.unop[G]
}
