package net.alasc.prep

import scala.util.Random

import org.scalacheck._
import org.scalatest.FunSuite

import spire.algebra.{Group, Order}
import spire.std.int._
import spire.syntax.partialAction._
import spire.syntax.order._
import spire.util.Opt

import net.alasc.algebra._
import net.alasc.domains._
import net.alasc.finite._
import net.alasc.laws.{Partitions, Permutations}
import net.alasc.named._
import net.alasc.perms._
import net.alasc.prep._
import net.alasc.prep.bsgs.{Chain, Node, Term}
import net.alasc.prep.chain._
import net.alasc.std.seq._
import net.alasc.syntax.all._

object Helpers {

  def generatorsSupportMax[G:PermutationAction](generators: Iterable[G]): Int =
    generators.foldLeft(-1) {
      case (mx, g) => spire.math.max(mx, g.supportMax.getOrElse(-1))
    }

  def groupSupportMax[G](grp: PGrp[G]): Int =
    generatorsSupportMax(grp.generators)(grp.pRep.permutationAction)

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

  def isNonDecreasing[A:Order](seq: Seq[A]): Boolean = {
    val pairs = seq.iterator zip seq.iterator.drop(1)
    pairs.forall { case (i, j) => i <= j }
  }

}

abstract class BSGSCheck(algoName: String)(implicit val builder: PGrpChainBuilder[Perm])  extends Properties("BaseGuideCheck/" + algoName) {

  import builder.{baseChange, baseSwap, schreierSims}

  import Helpers._

  val groups = Seq(
    RubikCube[Perm], Dihedral[Perm](8),
    Symmetric[Perm](5), Alternating[Perm](7),
    Mathieu[Perm](10), Mathieu[Perm](11),
    Mathieu[Perm](12), Mathieu[Perm](20)
  )

  val genChain: Gen[Chain[Perm]] =
    Gen.oneOf(groups).map(grp => builder.fromGrp(grp).chain)

  property("Base change guided by partition has base points corresponding to blocks of increasing size") = Prop.forAllNoShrink(genChain) { chain =>
    val n = chainSupportMax(chain) + 1
    Prop.forAll(Partitions.forDomain(Domain(n))) { partition =>
      val definition = bsgs.FixingPartition(Permutation[Perm], partition)
      val newChain = bsgs.BuildChain.fromChain(chain, Permutation[Perm], definition.baseGuideOpt)
      val baseBlockSize = newChain.base.map(partition.blockFor(_).size)
      isNonDecreasing(baseBlockSize)
    }
  }

  property("Base swap of the two base points") = Prop.forAllNoShrink(genChain) { chain =>
    Prop.forAll(genSwapIndex(chain)) {
      case Opt(index) =>
        val mutableChain = chain.mutableChain
        val node1 = mutableChain.mutable(mutableChain.start.next.nodesNext(index))
        val node2 = mutableChain.mutable(mutableChain.start.next.nodesNext(index + 1))
        builder.baseSwap.baseSwap(mutableChain, node1, node2)
        Prop(mutableChain.start.next.order == chain.order)
      case _ => Prop.undecided
    }
  }

  property("Put existing base point after") = Prop.forAllNoShrink(genChain) { chain =>
    Prop.forAll(genExistingBasePoint(chain)) {
      case Opt(k) =>
        val mutableChain = chain.mutableChain
        mutableChain.putExistingBasePointAfter(mutableChain.start, k)
        Prop(mutableChain.start.next.order == chain.order)
      case _ => Prop.undecided
    }
  }

  property("changeBase preserves order -- reordered base") = Prop.forAllNoShrink(genChain) { chain =>
    Prop.forAll(genSwappedSeq(chain.base)) { newBase =>
      val mutableChain = chain.mutableChain
      baseChange.changeBase(mutableChain, newBase)
      mutableChain.start.next.order == chain.order
    }
  }

  property("changeBase preserves order -- new base") = Prop.forAllNoShrink(genChain) { chain =>
    Prop.forAll(genNewBase(chain)) { newBase =>
      val mutableChain = chain.mutableChain
      baseChange.changeBase(mutableChain, newBase)
      mutableChain.start.next.order == chain.order
    }
  }

  property("conjugate and changeBase back = same orbits") = Prop.forAllNoShrink(genChain) { chain =>
    Prop.forAll(genRandomElement(chain)) { g =>
      val oldBase = chain.base
      val oldOrbitSizes = chain.nodesNext.map(_.orbitSize)
      val mutableChain = chain.mutableChain
      mutableChain.conjugate(g, g.inverse)
      mutableChain.check
      baseChange.changeBase(mutableChain, oldBase)
      val newOrbitSizes = mutableChain.start.next.nodesNext.map(_.orbitSize)
      oldOrbitSizes.sameElements(newOrbitSizes)
    }
  }

}

object BSGSCheck {

  val deterministic = {
    import PGrp.deterministic._
    implicitly[PGrpChainBuilder[Perm]]
  }

  val randomized = {
    import PGrp.default._
    implicitly[PGrpChainBuilder[Perm]]
  }

}

object BSGSCheckDeterministic extends BSGSCheck("deterministic")(BSGSCheck.deterministic)

object BSGSCheckRandomized extends BSGSCheck("randomized")(BSGSCheck.randomized)
