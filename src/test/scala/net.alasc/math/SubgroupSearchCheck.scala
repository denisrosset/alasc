package net.alasc.math

import org.scalacheck._
import scala.util.Random
import org.scalatest.FunSuite

import net.alasc.algebra._
import bsgs._
import algorithms._
import net.alasc.syntax.check._
import net.alasc.syntax.subgroup._
import net.alasc.syntax.shiftablePermutation._

object SubgroupSearchCheck extends Properties("SubgroupSearch") {
  val genAlg = for {
    useRandom <- Gen.oneOf(true, false)
    seed <- Gen.choose(0, 1000)
    alg = if (useRandom) BasicAlgorithms.randomized[Perm](new scala.util.Random(seed)) else BasicAlgorithms.deterministic[Perm]
  } yield alg

  val genSeq = for {
    n <- Gen.choose(1, 30)
    seq <- Gen.listOfN(n, Gen.choose(0, 4))
  } yield seq
    
  property("SubgroupSearch.fixing") = Prop.forAllNoShrink(genAlg, genSeq) { (alg, seq) =>
    implicit def nb = alg.nodeBuilder
    val chain1 = alg.completeChainFromSubgroup(FixingSeq[Perm](seq)).toChain
    val sym = Sym[Perm](seq.length)
    val symChain = alg.completeChainFromSubgroup(sym).toChain
    val partition = OldPartition.fromSeqHashCode(seq)
    val chain2 = alg.fixingPartition(symChain, partition).toChain
    chain1.order == chain2.order
  }
}
