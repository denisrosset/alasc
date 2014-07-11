package net.alasc

import spire.algebra.{GroupAction, Order, Semigroup}
import spire.syntax._
import scala.language.implicitConversions

/** Type class enabling access to a lexicographically sorted sequence of representatives of T
  * under the action of a permutation group of elements F.
  */
trait Lexico[P] {
  type A
  type F <: Finite[F]
  implicit def order: Order[A]
  implicit def index: Index[A, P]
  implicit def action: GroupAction[P, F]

  def baseGroup(p: P): Group[F]
  def symmetryGroup(p: P): Group[F]
  def permutationToFirst(p: P): F
}

/** Type class enabling access to a lexicographically sorted sequence of representatives of T
  * under the action of a permutation group of elements F.
  */
trait LexicoSeq[P] extends Lexico[P] {
  def permutationToIndex(p: P, index: BigInt): F
  def permutationIterator(p: P): Iterator[F]
}

final class LexicoOps[P](lhs: P)(implicit val lex: Lexico[P]) {
  def symmetryGroup: Group[lex.F] = lex.symmetryGroup(lhs)
  def numberOfRepresentatives: BigInt = lex.baseGroup(lhs).order / lex.symmetryGroup(lhs).order
  def permutationToFirst = lex.permutationToFirst(lhs)
  def lexFirst = lex.action.actr(lhs, permutationToFirst)
}

final class LexicoSeqOps[P](lhs: P)(implicit val lex: LexicoSeq[P]) {
  implicit def action = lex.action 
  def lexPermutations: BigIndexedSeq[lex.F] = new BigIndexedSeq[lex.F] {
    def head = lex.permutationToFirst(lhs)
    def iterator = lex.permutationIterator(lhs)
    def apply(index: BigInt) = lex.permutationToIndex(lhs, index)
    def length = lex.baseGroup(lhs).order / lex.symmetryGroup(lhs).order
  }
  def lexRepresentatives: BigIndexedSeq[P] = lexPermutations.map(lex.action.actr(lhs, _))
}
