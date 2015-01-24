package net.alasc.std

import scala.language.higherKinds

import scala.annotation.tailrec

import scala.collection.generic.CanBuildFrom
import scala.collection.SeqLike
import scala.collection.mutable
import scala.reflect.classTag

import spire.algebra._
import spire.algebra.lattice.{Lattice, BoundedJoinSemilattice}
import spire.syntax.eq._
import spire.syntax.group._
import spire.syntax.action._
import spire.util.Nullbox

import net.alasc.algebra._
import net.alasc.util._

class SeqSequence[SA <: SeqLike[A, SA], A] extends Sequence[SA, A] {
  def length(s: SA) = s.length
  def elemAt(s: SA, i: Int): A = s(i)
  def toIndexedSeq(s: SA): IndexedSeq[A] = s.toIndexedSeq
}

class SeqPermutationAction[SA <: SeqLike[A, SA], A, P: FiniteGroup: FaithfulPermutationAction](
  implicit cbf: CanBuildFrom[Nothing, A, SA]) extends NullboxPartialAction[SA, P] {
  import net.alasc.syntax.permutationAction._
  import spire.syntax.group._
  import spire.syntax.action._

  override def actlIsDefined(p: P, s: SA) = p.supportMax.getOrElseFast(-1) < s.length
  override def actrIsDefined(s: SA, p: P) = p.supportMax.getOrElseFast(-1) < s.length

  def partialActl(p: P, s: SA): Nullbox[SA] =
    if (p.supportMax.getOrElseFast(-1) >= s.length) Nullbox.empty[SA] else {
      val b = cbf()
      b.sizeHint(s)
      for (i <- 0 until s.length)
        b += s(i <|+| p)
      Nullbox(b.result)
    }

  def partialActr(s: SA, p: P): Nullbox[SA] = partialActl(p.inverse, s)
}

trait SeqInstances0 {
  implicit def SeqSequence[CC[A] <: SeqLike[A, CC[A]], A]: Sequence[CC[A], A] = new SeqSequence[CC[A], A]
  implicit def SeqPermutationAction[CC[A] <: SeqLike[A, CC[A]], A, P: FiniteGroup: FaithfulPermutationAction](
    implicit cbf: CanBuildFrom[Nothing, A, CC[A]]): PartialAction[CC[A], P] = new SeqPermutationAction[CC[A], A, P]
}

trait SeqInstances extends SeqInstances0 {

}
