package net.alasc

import spire.algebra.Order
import spire.implicits._
import spire.syntax.OrderOps
import permutable.{Permutable, PermutableImpl}
import indexSyntax._

object LexicoImpl {
  def computeIntegerSeq[A: Order](seq: IndexedSeq[A]): IndexedSeq[Int] = {
    import spire.algebra.Order.ordering
    val elementMap = seq.distinct.sorted.zipWithIndex.toMap
    seq.map(elementMap(_))
  }
}

trait LexicoImpl[P] extends Lexico[P] {
  self =>
  def symmetryGroup(p: P) = baseGroup(p).fixing(integerSeq(p))

  /** The `integerSeq` is the sequence used for the lexicographic ordering.
    * Instead of dealing with coefficients of type A, we work with
    * their relative orders expressed as integers.
    */
  def integerSeq(p: P): IndexedSeq[Int] = LexicoImpl.computeIntegerSeq(p.indexToIndexedSeq)

  trait PermutableTrait[PP <: PermutableTrait[PP]] extends Permutable[PP, F] {
    def p: P
    def build(newP: P): PP
    def permutedBy(f: F): PP = build(action.actr(p, f))
    val integerSeq: IndexedSeq[Int] = self.integerSeq(p)
    trait PermutationsTrait extends Permutations {
      val baseGroup: Group[F] = self.baseGroup(p)
    }
  }
}

trait BruteForceLexicoImpl[P] extends LexicoImpl[P] {
  def permutationToFirst(p: P) = PermutableWrap(p).Perms.minimalPermutation
  case class PermutableWrap(p: P) extends PermutableTrait[PermutableWrap]
      with PermutableImpl[PermutableWrap, F] {
    def build(newP: P) = PermutableWrap(newP)
    object Perms extends PermutationsTrait with BruteForcePermutations
  }
}

trait WithoutSymmetrySubgroupLexicoImpl[P] extends LexicoImpl[P] {
  def permutationToFirst(p: P) = PermutableWrap(p).Perms.minimalPermutation
  case class PermutableWrap(p: P) extends PermutableTrait[PermutableWrap]
      with PermutableImpl[PermutableWrap, F] {
    def build(newP: P) = PermutableWrap(newP)
    object Perms extends PermutationsTrait with WithoutSymmetrySubgroupPermutations
  }
}
trait BigSeqLexicoImpl[P] extends LexicoImpl[P] with LexicoSeq[P] {
  def permutationToFirst(p: P): F = PermutableWrap(p).Perms.minimalPermutation
  def permutationIterator(p: P): Iterator[F] = PermutableWrap(p).Perms.permutationIterator
  def permutationToIndex(p: P, index: BigInt) = PermutableWrap(p).Perms.permutationForIndex(index)
  case class PermutableWrap(p: P) extends PermutableTrait[PermutableWrap]
      with PermutableImpl[PermutableWrap, F] {
    def build(newP: P) = PermutableWrap(newP)
    object Perms extends PermutationsTrait with BigSeqPermutations
  }
}
