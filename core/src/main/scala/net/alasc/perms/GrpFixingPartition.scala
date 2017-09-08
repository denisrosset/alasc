package net.alasc.perms

import spire.math.SafeLong
import net.alasc.algebra._
import net.alasc.partitions.Partition
import net.alasc.finite.{Grp, GrpGroup}
import net.alasc.named.Symmetric

object GrpFixingPartition {

  def blockOrder(block: Partition#Block): SafeLong = Symmetric.order(block.size)

  def blockGenerators(block: Partition#Block): Seq[Perm] =
    if (block.size <= 1) IndexedSeq.empty else {
      val builder = IndexedSeq.newBuilder[Perm]
      builder.sizeHint(block.size - 1)
      var k = block.start
      while (block.hasNext(k)) {
        val k1 = block.next(k)
        builder += Perm.transposition(k, k1)
        k = k1
      }
      builder.result()
    }

  def order(partition: Partition): SafeLong =
    (SafeLong.one /: partition.blocks) { case (o, block) => o * blockOrder(block) }

  def generators(partition: Partition): Seq[Perm] =
    partition.blocks.flatMap(blockGenerators(_)).toIndexedSeq

  def apply(partition: Partition)(implicit ev: GrpGroup[Perm]): Grp[Perm] =
    Grp.fromGeneratorsAndOrder(generators(partition), order(partition))

}
