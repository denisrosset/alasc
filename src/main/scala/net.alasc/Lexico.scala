package net.alasc

import spire.algebra.{GroupAction, Order, Semigroup}
import spire.syntax._
import scala.language.implicitConversions

/** Base type class to access the lexicographically sorted sequence of
  * representatives of T under the action of a permutation group of elements F.
  */
trait Lexico[P, F <: Finite[F]] {
  type A
  implicit def order: Order[A]
  implicit def index: Index[P, A]
  implicit def action: GroupAction[P, F]

  def baseGroup(p: P): Group[F]
  def symmetryGroup(p: P): Group[F]
}

/** Type class enabling access to the first element in the lexicographic sorted sequence. */
trait LexicoFirst[P, F <: Finite[F]] extends Lexico[P, F] {
  def permutationToFirst(p: P): F
}

/** Type class enabling access to each element in the lexicographic sorted sequence. */
trait LexicoSeq[P, F <: Finite[F]] extends Lexico[P, F] {
  def permutationToIndex(p: P, index: BigInt): F
  def permutationIterator(p: P): Iterator[F]
}

final class LexicoFirstOps[P, F <: Finite[F]](lhs: P)(implicit val lex: LexicoFirst[P, F]) {
  def lexSymmetryGroup: Group[F] = lex.symmetryGroup(lhs)
  def numberOfRepresentatives: BigInt = lex.baseGroup(lhs).order / lex.symmetryGroup(lhs).order
  def permutationToFirst = lex.permutationToFirst(lhs)
  def lexFirst = lex.action.actr(lhs, permutationToFirst)
}

final class LexicoSeqOps[P, F <: Finite[F]](lhs: P)(implicit val lex: LexicoSeq[P, F]) {
  def lexPermutations: BigIndexedSeq[F] = new BigIndexedSeq[F] {
    def iterator = lex.permutationIterator(lhs)
    def apply(index: BigInt) = lex.permutationToIndex(lhs, index)
    def length = lex.baseGroup(lhs).order / lex.symmetryGroup(lhs).order
  }
  def lexRepresentatives: BigIndexedSeq[P] = lexPermutations.map(lex.action.actr(lhs, _))
}
