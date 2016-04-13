package net.alasc
package syntax

import scala.language.experimental.macros
import scala.reflect.ClassTag
import scala.util.Random

import spire.macros.Ops
import spire.algebra.Monoid

import net.alasc.algebra._
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
