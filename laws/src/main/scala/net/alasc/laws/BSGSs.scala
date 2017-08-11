package net.alasc.laws

import scala.util.Random

import spire.algebra.{Action, Group, Order}
import spire.util.Opt

import org.scalacheck.Gen
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.matchers.{MatchResult, Matcher}
import spire.syntax.action._

import net.alasc.algebra.PermutationAction
import net.alasc.bsgs.{Chain, Node, Term}
import net.alasc.std.seq._
import spire.syntax.order._

import net.alasc.perms.Perm

object BSGSs {

  def genExistingBasePoint(chain: Chain[_, _]): Gen[Opt[Int]] = {
    val base = chain.base
    if (base.isEmpty) Gen.const(Opt.empty[Int]) else Gen.oneOf(base).map(k => Opt(k))
  }

  def genSwapIndex(chain: Chain[_, _]): Gen[Opt[Int]] =
    if (chain.length < 2)
      Gen.const(Opt.empty[Int])
    else
      Gen.choose(0, chain.length - 2).map(i => Opt(i))

  def genSwappedSeq[A](seq: Seq[A]): Gen[Seq[A]] =
    Permutations.permForSize(seq.size).map( perm => (seq <|+| perm) )

  def genNewBase[G, F <: PermutationAction[G] with Singleton](chain: Chain[G, F]): Gen[Seq[Int]] = chain match {
    case _: Term[G, F] => Gen.const(Seq.empty[Int])
    case node: Node[G, F] =>
      val n = node.action.largestMovedPoint(node.strongGeneratingSet).getOrElse(0) + 1
      genSwappedSeq(0 until n).flatMap(full => Gen.choose(0, n - 1).map(m => full.take(m)))
  }

  def genRandomElement[G:Group](chain: Chain[G, _]): Gen[G] =
    arbitrary[Long].map( seed => chain.randomElement(new Random(seed)))

  def beWeaklyIncreasing[A:Order] = new Matcher[Seq[A]] {

    def apply(left: Seq[A]) = {
      val pairs = left.iterator zip left.iterator.drop(1)
      MatchResult(
        pairs.forall { case (i, j) => i <= j },
        s"""Sequence $left was not (weakly) increasing""",
        s"""Sequence $left was (weakly) increasing"""
      )
    }

  }

}
