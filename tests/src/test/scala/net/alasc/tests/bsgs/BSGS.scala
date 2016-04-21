package net.alasc.tests
package bsgs

import org.scalacheck._
import spire.algebra.{Group, Order}
import spire.util.Opt

import org.scalatest.matchers.{MatchResult, Matcher}

import net.alasc.algebra._
import net.alasc.bsgs.{BuildChain, Chain, Node, Term}
import net.alasc.domains._
import net.alasc.finite.Grp
import net.alasc.laws.{Partitions, Permutations}
import net.alasc.named._
import net.alasc.perms._
import net.alasc.perms.chain.PermGrpChainBuilder
import net.alasc.bsgs.{FixingPartition => FixingPartitionDef, _}

abstract class BSGSSuite(implicit val builder: PermGrpChainBuilder[Perm]) extends AlascSuite {

  import BSGSSuite._

  import builder.{baseChange, baseSwap, schreierSims}

  val groups = Seq(
    RubikCube[Perm], Dihedral[Perm](8),
    Symmetric[Perm](5), Alternating[Perm](7),
    Mathieu[Perm](10), Mathieu[Perm](11),
    Mathieu[Perm](12), Mathieu[Perm](20)
  )

  val genChain: Gen[Chain[Perm]] =
    Gen.oneOf(groups).map(grp => builder.fromGrp(grp).chain)

  implicit val noShrinkChain = noShrink[Chain[Perm]]

  test("Base change guided by partition has base points corresponding to blocks of increasing size") {
    forAll(genChain) { chain =>
      val n = chainSupportMax(chain) + 1
      val domain = Domain(n)
      forAll(Partitions.forDomain(domain)) { partition =>
        val definition = FixingPartitionDef[Perm](implicitly, partition)
        val newChain = BuildChain.fromChain(chain, PermutationBuilder[Perm], definition.baseGuideOpt)
        val baseBlockSize = newChain.base.map(partition.blockFor(_).size)
        baseBlockSize should beWeaklyIncreasing[Int]
      }
    }
  }

  test("Base swap of the two base points") {
    forAll(genChain) { chain =>
      forAll(genSwapIndex(chain)) {
        case Opt(index) =>
          val mutableChain = chain.mutableChain
          val nodeSeq = mutableChain.start.next.nodesIterator.toSeq
          val node1 = mutableChain.mutable(nodeSeq(index))
          val node2 = mutableChain.mutable(nodeSeq(index + 1))
          builder.baseSwap.baseSwap(mutableChain, node1, node2)
          mutableChain.start.next.order should === (chain.order)
        case _ => discardEvaluation()
      }
    }
  }

  test("Put existing base point after") {
    forAll(genChain) { chain =>
      forAll(genExistingBasePoint(chain)) {
        case Opt(k) =>
          val mutableChain = chain.mutableChain
          mutableChain.putExistingBasePointAfter(mutableChain.start, k)
          mutableChain.start.next.order should === (chain.order)
        case _ => discardEvaluation()
      }
    }
  }

  test("changeBase preserves order -- reordered base") {
    forAll(genChain) { chain =>
      forAll(genSwappedSeq(chain.base)) { newBase =>
        val mutableChain = chain.mutableChain
        baseChange.changeBase(mutableChain, newBase)
        mutableChain.start.next.order should === (chain.order)
      }
    }
  }

  test("changeBase preserves order -- new base") {
    forAll(genChain) { chain =>
      forAll(genNewBase(chain)) { newBase =>
        val mutableChain = chain.mutableChain
        baseChange.changeBase(mutableChain, newBase)
        mutableChain.start.next.order should === (chain.order)
      }
    }
  }

  test("conjugate and changeBase back = same orbits") {
    forAll(genChain) { chain =>
      forAll(genRandomElement(chain)) { g =>
        val oldBase = chain.base
        val oldOrbitSizes = chain.nodesIterator.map(_.orbitSize).toSeq
        val mutableChain = chain.mutableChain
        mutableChain.conjugate(g, g.inverse)
        mutableChain.check
        baseChange.changeBase(mutableChain, oldBase)
        val newOrbitSizes = mutableChain.start.next.nodesIterator.map(_.orbitSize).toSeq
        oldOrbitSizes should === (newOrbitSizes)
      }
    }
  }

}

object BSGSSuite {

  import net.alasc.syntax.all._
  import net.alasc.std.any._
  import spire.syntax.order._
  import spire.syntax.partialAction._

  def generatorsSupportMax[G:PermutationAction](generators: Iterable[G]): Int =
    generators.foldLeft(-1) {
      case (mx, g) => spire.math.max(mx, g.largestMovedPoint.getOrElse(-1))
    }

  def groupSupportMax[G:Permutation](grp: Grp[G]): Int =
    grp.largestMovedPoint.getOrElseFast(-1)

  def chainSupportMax[G](chain: Chain[G]): Int = chain match {
    case _: Term[G] => -1
    case node: Node[G] =>
      generatorsSupportMax(node.strongGeneratingSet)(node.action)
  }

  def genExistingBasePoint[G](chain: Chain[G]): Gen[Opt[Int]] = {
    val base = chain.base
    if (base.isEmpty) Gen.const(Opt.empty[Int]) else Gen.oneOf(base).map(k => Opt(k))
  }

  def genSwapIndex[G](chain: Chain[G]): Gen[Opt[Int]] =
    if (chain.length < 2)
      Gen.const(Opt.empty[Int])
    else
      Gen.choose(0, chain.length - 2).map(i => Opt(i))

  def genSwappedSeq[A](seq: Seq[A]): Gen[Seq[A]] =
    Permutations.forSize[Perm](seq.size).map( perm => (seq <|+|? perm).get )

  def genNewBase[G](chain: Chain[G]): Gen[Seq[Int]] = chain match {
    case _: Term[G] => Gen.const(Seq.empty[Int])
    case node: Node[G] =>
      val n = generatorsSupportMax(chain.strongGeneratingSet)(node.action) + 1
      genSwappedSeq(0 until n).flatMap( full => Gen.choose(0, n - 1).map( m => full.take(m) ) )
  }

  def genRandomElement[G:Group](chain: Chain[G]): Gen[G] =
    Gen.parameterized( p => chain.randomElement(p.rng) )

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

  val deterministic = {
    import net.alasc.perms.deterministic._
    implicitly[PermGrpChainBuilder[Perm]]
  }

  val randomized = {
    import net.alasc.perms.default._
    implicitly[PermGrpChainBuilder[Perm]]
  }

}

final class BSGSSuiteDeterministic extends BSGSSuite()(BSGSSuite.deterministic)

final class BSGSSuiteRandomized extends BSGSSuite()(BSGSSuite.randomized)
