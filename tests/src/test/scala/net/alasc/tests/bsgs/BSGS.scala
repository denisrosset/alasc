package net.alasc.tests
package bsgs

import org.scalacheck._
import spire.util.Opt

import net.alasc.algebra._
import net.alasc.bsgs.{BuildChain, Chain}
import net.alasc.domains._
import net.alasc.laws.{BSGSs, Partitions}
import net.alasc.named._
import net.alasc.perms.{_}
import net.alasc.bsgs.{FixingPartition => FixingPartitionDef, _}
import net.alasc.finite.Grp
import net.alasc.tests.perms.PermSuite

abstract class BSGSSuite(implicit val builder: GrpPermAlgorithms) extends AlascSuite {

  import BSGSs._

  import builder.{baseChange, baseSwap, schreierSims}

  type F = Perm.algebra.type
  implicit def F: F = Perm.algebra

  val groups: Seq[Grp[Perm]] = Seq(
    RubikCube(), Dihedral(8),
    Symmetric(5), Alternating(7),
    Mathieu(10), Mathieu(11),
    Mathieu(12), Mathieu(20)
  )

  val genChain: Gen[Chain[Perm, Perm.algebra.type]] =
    Gen.oneOf(groups).map(grp => builder.fromGrp(grp).chain)

  implicit val noShrinkChain = noShrink[Chain[Perm, F]]

  test("Base change guided by partition has base points corresponding to blocks of increasing size") {
    forAll(genChain) { chain =>
      val n = F.largestMovedPoint(chain.strongGeneratingSet).getOrElseFast(0) + 1
      val domain = Domain(n)
      forAll(Partitions.forDomain(domain)) { partition =>
        val definition = FixingPartitionDef[Perm, F](partition)
        val newChain = BuildChain.fromChain[Perm, F, F](chain, definition.baseGuideOpt)
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

final class BSGSSuiteDeterministic extends BSGSSuite()(PermSuite.deterministic)

final class BSGSSuiteRandomized extends BSGSSuite()(PermSuite.randomized)
