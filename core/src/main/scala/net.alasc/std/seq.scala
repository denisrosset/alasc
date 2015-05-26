package net.alasc.std

import scala.language.higherKinds

import scala.annotation.tailrec

import scala.collection.generic.CanBuildFrom
import scala.collection.SeqLike

import spire.algebra._
import spire.algebra.partial._
import spire.syntax.cfor._
import spire.syntax.group._
import spire.syntax.action._
import spire.util.Opt

import net.alasc.algebra._
import net.alasc.syntax.permutationAction._
import net.alasc.util._

class SeqPermutationAction[SA <: SeqLike[A, SA], A, P: FiniteGroup: FaithfulPermutationAction](
  implicit cbf: CanBuildFrom[Nothing, A, SA]) extends PartialAction[SA, P] {

  override def actlIsDefined(p: P, s: SA) = p.supportMax.getOrElseFast(-1) < s.length
  override def actrIsDefined(s: SA, p: P) = p.supportMax.getOrElseFast(-1) < s.length

  def partialActl(p: P, s: SA): Opt[SA] =
    if (p.supportMax.getOrElseFast(-1) >= s.length) Opt.empty[SA] else {
      val b = cbf()
      b.sizeHint(s)
      cforRange(0 until s.length) { i =>
        b += s(i <|+| p)
      }
      Opt(b.result)
    }

  def partialActr(s: SA, p: P): Opt[SA] = partialActl(p.inverse, s)
}

trait SeqInstances0 {
  implicit def SeqPermutationAction[CC[A] <: SeqLike[A, CC[A]], A, P: FiniteGroup: FaithfulPermutationAction](
    implicit cbf: CanBuildFrom[Nothing, A, CC[A]]): PartialAction[CC[A], P] = new SeqPermutationAction[CC[A], A, P]
}

trait SeqInstances extends SeqInstances0 {

}
