package net.alasc.perms

import scala.reflect.ClassTag
import scala.util.Random

import spire.algebra.{Eq, Group}
import spire.syntax.group._
import spire.syntax.eq._
import spire.util.Opt

import net.alasc.algebra._
import net.alasc.domains.Partition
import net.alasc.finite._
import net.alasc.named.Symmetric
import net.alasc.prep._

object FixingPartition {

  def blockOrder(block: Partition#Block): BigInt = Symmetric.order(block.size)

  def blockGenerators[G:Permutation](block: Partition#Block): Iterable[G] =
    if (block.size <= 1) Iterable.empty else {
      val builder = IndexedSeq.newBuilder[G]
      builder.sizeHint(block.size - 1)
      var k = block.start
      while (block.hasNext(k)) {
        val k1 = block.next(k)
        builder += Permutation[G].transposition(k, k1)
        k = k1
      }
      builder.result()
    }

  def order(partition: Partition): BigInt =
    (BigInt(1) /: partition.blocks) { case (o, block) => o * blockOrder(block) }

  def generators[G:Permutation](partition: Partition): Iterable[G] =
    partition.blocks.flatMap(blockGenerators[G](_))

  def apply[G:Permutation:PGrpBuilder](partition: Partition): PGrp[G] =
    PGrpBuilder[G].fromGeneratorsAndOrder(generators[G](partition), order(partition))

}
