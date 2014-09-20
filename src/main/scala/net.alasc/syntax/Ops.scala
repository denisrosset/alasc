package net.alasc
package syntax

import scala.language.experimental.macros

import scala.util.Random

import machinist.{DefaultOps => Ops}

import net.alasc.algebra._
import net.alasc.math.Grp
import net.alasc.util._

final class LatticeOps[A](lhs: A)(implicit ev: Lattice[A]) {
  def meet(rhs: A): A = macro Ops.binop[A, A]
  def join(rhs: A): A = macro Ops.binop[A, A]
}

final class CheckOps[A](lhs: A)(implicit ev: Check[A]) {
  def check(): Unit = ev.check(lhs)
}

final class SequenceOps[T, A](lhs: T)(implicit ev: Sequence[T, A]) {
  def length(): Int = macro Ops.unop[Int]
  def elemAt(rhs: Int): A = macro Ops.binop[Int, A]
  def toIndexedSeq(): IndexedSeq[A] = macro Ops.unop[IndexedSeq[A]]
}

final class FiniteGroupOps[A](lhs: A)(implicit ev: FiniteGroup[A]) {
  def order(): Int = macro Ops.unop[Int]
  def conjBy(rhs: InversePair[A]): A = macro Ops.binop[InversePair[A], A]
}

final class PermutationActionOps[A](lhs: A)(implicit ev: PermutationAction[A]) {
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

final class SubgroupOps[S, G](lhs: S)(implicit ev: Subgroup[S, G]) {
  def iterator(): Iterator[G] = macro Ops.unop[Iterator[G]]
  def elements(): coll.Set[G] = macro Ops.unop[coll.Set[G]]
  def generators(): Iterable[G] = macro Ops.unop[Iterable[G]]
  def order(): BigInt = macro Ops.unop[BigInt]
  def randomElement(rhs: Random): G = macro Ops.binop[Random, G]
  def contains(rhs: G): Boolean = macro Ops.binop[G, Boolean]
  def toGrp()(implicit representations: Representations[G]): Grp[G] = ev.toGrp(lhs)
}

final class PermutationSubgroupOps[S, G](lhs: S)(implicit ev: Subgroup[S, G], action: FaithfulPermutationAction[G]) {
  def supportMin(): NNOption = ev.supportMin(lhs)
  def supportMax(): NNOption = ev.supportMax(lhs)
  def supportAny(): NNOption = ev.supportAny(lhs)
}
