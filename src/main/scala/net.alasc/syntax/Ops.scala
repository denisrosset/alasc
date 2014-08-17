package net.alasc.syntax

import scala.collection.immutable.BitSet
import scala.language.experimental.macros

import scala.util.Random

import machinist.{DefaultOps => Ops}

import net.alasc.algebra._
import net.alasc.util._

final class CheckOps[A](lhs: A)(implicit ev: Check[A]) {
  def check(): Unit = ev.check(lhs)
}

final class LengthOps[A](lhs: A)(implicit ev: Length[A]) {
  def length(): Int = macro Ops.unop[Int]
}

final class BigLengthOps[A](lhs: A)(implicit ev: BigLength[A]) {
  def bigLength(): BigInt = macro Ops.unop[BigInt]
}

final class IndexOps[T, A](lhs: T)(implicit ev: Index[T, A]) {
  def element(rhs: Int): A = macro Ops.binop[Int, A]
  def toIndexedSeq(): IndexedSeq[A] = macro Ops.unop[IndexedSeq[A]]
}

final class BigIndexOps[T, A](lhs: T)(implicit ev: BigIndex[T, A]) {
  def bigElement(rhs: BigInt): A = macro Ops.binop[BigInt, A]
}

final class FiniteGroupOps[A](lhs: A)(implicit ev: FiniteGroup[A]) {
  def order(): Int = macro Ops.unop[Int]
}

final class PermutationActionOps[A](lhs: A)(implicit ev: PermutationAction[A]) {
  def support(): BitSet = macro Ops.unop[BitSet]
  def supportMax(): NNOption = macro Ops.unop[NNOption]
  def supportMin(): NNOption = macro Ops.unop[NNOption]
  def supportAny(): NNOption = macro Ops.unop[NNOption]
  def to[Q](implicit evQ: Permutation[Q]): Q = ev.to[Q](lhs)
}

final class ShiftablePermutationOps[A](lhs: A)(implicit ev: ShiftablePermutation[A]) {
  def +(rhs: Int): A = macro Ops.binop[Int, A]
  def -(rhs: Int): A = macro Ops.binop[Int, A]
}

final class SubgroupOps[S, G](lhs: S)(implicit ev: Subgroup[S, G]) {
  def elements(): Iterable[G] = macro Ops.unop[Iterable[G]]
  def generators(): Iterable[G] = macro Ops.unop[Iterable[G]]
  def order(): BigInt = macro Ops.unop[BigInt]
  def randomElement(rhs: Random): G = macro Ops.binop[Random, G]
  def contains(rhs: G): Boolean = macro Ops.binop[G, Boolean]
}

final class PermutationSubgroupOps[S, G](lhs: S)(implicit ev: Subgroup[S, G]) {
  def supportMin(): NNOption = macro Ops.unop[NNOption]
  def supportMax(): NNOption = macro Ops.unop[NNOption]
  def supportAny(): NNOption = macro Ops.unop[NNOption]
}
