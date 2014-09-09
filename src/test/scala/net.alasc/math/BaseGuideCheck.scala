package net.alasc.math

import org.scalacheck._
import scala.util.Random
import org.scalatest.FunSuite
import bsgs._
import algorithms._
import net.alasc.syntax.check._
import net.alasc.syntax.subgroup._
import net.alasc.syntax.permutationSubgroup._

object BaseGuideGenerators {
  import MathieuGroups._
  val genGroupAndPartition = for {
    (generators, order) <- Gen.oneOf(M11, M12, M20, M21, M22, M23, RubikCube.group)
    grp = Grp.fromGeneratorsAndOrder(generators, order)
    n = grp.supportMax.getOrElse(1) + 1
    seq <- Gen.listOfN(n, Gen.choose(0, 2))
  } yield (grp, seq)
}

object BaseGuideCheck extends Properties("BSGS") {
  import BaseGuideGenerators._

  property("Base change guided by partition has base points corresponding to blocks of decreasing size") = Prop.forAllNoShrink(genGroupAndPartition) {
    case (grp, seq) => {
      import grp.algorithms
      val partition = Partition.fromSeqHashCode(seq).sizeDecreasing
      val chain = algorithms.withBase(grp.chain, PartitionGuide(partition))
      val baseBlockSize = chain.base.map(partition.blockFor(_).size)
      (baseBlockSize.iterator zip baseBlockSize.iterator.drop(1)).forall { case (i, j) => i <= j }
    }
  }
}
