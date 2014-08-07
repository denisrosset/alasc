package net.alasc.syntax

import net.alasc.algebra._
import scala.collection.immutable.BitSet
import scala.language.experimental.macros
import machinist.{DefaultOps => Ops}
import scala.util.Random

final class LengthOps[A](lhs: A)(implicit ev: Length[A]) {
  def length(): Int = macro Ops.unop[Int]
}

final class BigLengthOps[A](lhs: A)(implicit ev: BigLength[A]) {
  def bigLength(): BigInt = macro Ops.unop[BigInt]
}

final class IndexOps[A, T](lhs: T)(implicit ev: Index[A, T]) {
  def element(rhs: Int): A = macro Ops.binop[Int, A]
  def toIndexedSeq(): IndexedSeq[A] = macro Ops.unop[IndexedSeq[A]]
}

final class BigIndexOps[A, T](lhs: T)(implicit ev: BigIndex[A, T]) {
  def bigElement(rhs: BigInt): A = macro Ops.binop[BigInt, A]
}

final class FiniteGroupOps[A](lhs: A)(implicit ev: FiniteGroup[A]) {
  def isId(): Boolean = macro Ops.unop[Boolean]
  def order(): Int = macro Ops.unop[Int]
}

final class PermutationOps[A](lhs: A)(implicit ev: Permutation[A]) {
  def support(): BitSet = macro Ops.unop[BitSet]
  def supportMax(): Int = macro Ops.unop[Int]
  def supportMin(): Int = macro Ops.unop[Int]
  def to[Q](implicit evQ: BuildablePermutation[Q]): Q = ev.to[Q](lhs)
}

final class ShiftablePermutationOps[A](lhs: A)(implicit ev: ShiftablePermutation[A]) {
  def +(rhs: Int): A = macro Ops.binop[Int, A]
  def -(rhs: Int): A = macro Ops.binop[Int, A]
}

final class SubgroupOps[S, G](lhs: S)(implicit ev: Subgroup[S, G]) {
  def elements(): Iterable[G] = macro Ops.unop[Iterable[G]]
  def generators(): Seq[G] = macro Ops.unop[Seq[G]]
  def order(): BigInt = macro Ops.unop[BigInt]
  def random(rhs: Random): G = macro Ops.binop[Random, G]
  def contains(rhs: G): Boolean = macro Ops.binop[G, Boolean]
}

final class PermutationSubgroupOps[S, G](lhs: S)(implicit ev: Subgroup[S, G]) {
  def supportMin(): Int = macro Ops.unop[Int]
  def supportMax(): Int = macro Ops.unop[Int]
}
