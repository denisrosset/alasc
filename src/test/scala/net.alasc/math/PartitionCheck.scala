package net.alasc.math

import scala.util.Random

import org.scalacheck._
import org.scalatest.FunSuite

import spire.syntax.partialOrder._

import net.alasc.syntax.lattice._

object PartitionCheck extends Properties("SubgroupSearch") {
  val genPartition2 = for {
    size <- Gen.choose(1, 60)
    seq1 <- Gen.containerOfN[Seq, Int](size, Gen.choose(0, 4))
    seq2 <- Gen.containerOfN[Seq, Int](size, Gen.choose(0, 4))
  } yield (Domain(size).Partition.fromSeq(seq1), Domain(size).Partition.fromSeq(seq2))

  val genPartition3 = for {
    size <- Gen.choose(1, 60)
    seq1 <- Gen.containerOfN[Seq, Int](size, Gen.choose(0, 4))
    seq2 <- Gen.containerOfN[Seq, Int](size, Gen.choose(0, 4))
    seq3 <- Gen.containerOfN[Seq, Int](size, Gen.choose(0, 4))
  } yield (Domain(size).Partition.fromSeq(seq1), Domain(size).Partition.fromSeq(seq2), Domain(size).Partition.fromSeq(seq3))

  property("Partition.join") = Prop.forAllNoShrink(genPartition2) { case (gpart1, gpart2) =>
    val domain = gpart1.domain
    import domain.lattice
    val domain.Partition(part1) = gpart1
    val domain.Partition(part2) = gpart2
    val jn = part1 join part2
    (part1 <= jn) && (part2 <= jn)
  }

  property("Partition.meet") = Prop.forAllNoShrink(genPartition2) { case (gpart1, gpart2) =>
    val domain = gpart1.domain
    import domain.lattice
    val domain.Partition(part1) = gpart1
    val domain.Partition(part2) = gpart2
    val mt = part1 meet part2
    (mt <= part1) && (mt <= part2)
  }
}
