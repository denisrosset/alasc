package net.alasc
package syntax

import scala.language.experimental.macros
import scala.reflect.ClassTag
import scala.util.Random

import net.alasc.macros.Ops
import spire.algebra.Monoid

import net.alasc.algebra._
import net.alasc.math.Grp
import net.alasc.util._

final class CheckOps[A](lhs: A)(implicit ev: Check[A]) {
  def check(): Checked = ev.check(lhs)
}

final class MonoidOps[A](lhs: TraversableOnce[A])(implicit ev: Monoid[A]) {
  def combine(): A = macro Ops.unop[A]
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

final class SubgroupOps[S, G](lhs: S)(implicit ev: Subgroup[S, G]) {
  def hasSubgroup(rhs: Grp[G]): Boolean = macro Ops.binop[Grp[G], Boolean]
  def hasProperSubgroup(rhs: Grp[G]): Boolean = macro Ops.binop[Grp[G], Boolean]
  def isSubgroupOf(rhs: Grp[G]): Boolean = macro Ops.binop[Grp[G], Boolean]
  def isProperSubgroupOf(rhs: Grp[G]): Boolean = macro Ops.binop[Grp[G], Boolean]

  def iterator(): Iterator[G] = macro Ops.unop[Iterator[G]]
  def elements(): coll.Set[G] = macro Ops.unop[coll.Set[G]]
  def generators(): Iterable[G] = macro Ops.unop[Iterable[G]]
  def order(): BigInt = macro Ops.unop[BigInt]
  def randomElement(rhs: Random): G = macro Ops.binop[Random, G]
  def contains(rhs: G): Boolean = macro Ops.binop[G, Boolean]
  def toGrp()(implicit gClassTag: ClassTag[G], representations: Representations[G]): Grp[G] = ev.toGrp(lhs)
}

final class PermutationSubgroupOps[S, G](lhs: S)(implicit ev: Subgroup[S, G], action: FaithfulPermutationAction[G]) {
  def supportMin(): NNOption = ev.supportMin(lhs)
  def supportMax(): NNOption = ev.supportMax(lhs)
  def supportAny(): NNOption = ev.supportAny(lhs)
}

final class SemigroupoidOps[G](lhs: G)(implicit ev: Semigroupoid[G]) {
  def |+|? (rhs: G): Option[G] = macro Ops.binop[G, Option[G]]
  def |+|! (rhs: G): G = macro Ops.binop[G, G]
  def ?+? (rhs: G): Boolean = macro Ops.binop[G, Boolean]
}

final class WithBaseSemigroupoidOps[G, B](lhs: G)(implicit ev: WithBase[G, B]) {
  def source(): B = macro Ops.unop[B]
  def target(): B = macro Ops.unop[B]
}

final class PartialMonoidOps[G](lhs: G)(implicit ev: PartialMonoid[G]) {
  def isId(): Boolean = macro Ops.unop[Boolean]
  def leftId(): G = macro Ops.unop[G]
  def rightId(): G = macro Ops.unop[G]
}

final class PartialMonoidWithBaseOps[G, B](lhs: B)(implicit ev: PartialMonoidWithBase[G, B]) {
  def id(): G = macro Ops.unop[G]
}

final class GroupoidOps[G](lhs: G)(implicit ev: Groupoid[G]) {
  def inverse(): G = macro Ops.unop[G]
  def |-|? (rhs: G): Option[G] = macro Ops.binop[G, Option[G]]
  def |-|! (rhs: G): G = macro Ops.binop[G, G]
  def ?-? (rhs: G): Boolean = macro Ops.binop[G, Boolean]
}

final class PartialActionGroupOps[G](lhs: G) {
  def ?|+|> [P](rhs: P)(implicit ev: PartialAction[P, G]): Option[P] =
    macro Ops.binopWithEv[P, PartialAction[P, G], Option[P]]
  def ?+|> [P](rhs: P)(implicit ev: PartialAction[P, G]): Boolean =
    macro Ops.binopWithEv[P, PartialAction[P, G], Boolean]
  def !|+|> [P](rhs: P)(implicit ev: PartialAction[P, G]): P =
    macro Ops.binopWithEv[P, PartialAction[P, G], P]
}

final class PartialActionPointOps[P](lhs: P) {
  def <|+|? [G](rhs: G)(implicit ev: PartialAction[P, G]): Option[P] =
    macro Ops.binopWithEv[G, PartialAction[P, G], Option[P]]
  def <|+? [G](rhs: G)(implicit ev: PartialAction[P, G]): Boolean =
    macro Ops.binopWithEv[G, PartialAction[P, G], Boolean]
  def <|+|! [G](rhs: G)(implicit ev: PartialAction[P, G]): P =
    macro Ops.binopWithEv[G, PartialAction[P, G], P]
}
