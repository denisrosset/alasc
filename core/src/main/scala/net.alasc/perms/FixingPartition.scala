package net.alasc.perms

import net.alasc.algebra._
import net.alasc.domains.Partition
import net.alasc.finite.Grp
import net.alasc.named.Symmetric

object FixingPartition {

  def blockOrder(block: Partition#Block): BigInt = Symmetric.order(block.size)

  def blockGenerators[G:PermutationBuilder](block: Partition#Block): Iterable[G] =
    if (block.size <= 1) Iterable.empty else {
      val builder = IndexedSeq.newBuilder[G]
      builder.sizeHint(block.size - 1)
      var k = block.start
      while (block.hasNext(k)) {
        val k1 = block.next(k)
        builder += PermutationBuilder[G].transposition(k, k1)
        k = k1
      }
      builder.result()
    }

  def order(partition: Partition): BigInt =
    (BigInt(1) /: partition.blocks) { case (o, block) => o * blockOrder(block) }

  def generators[G:PermutationBuilder](partition: Partition): Iterable[G] =
    partition.blocks.flatMap(blockGenerators[G](_))

  def apply[G:PermutationBuilder:PermGrpBuilder](partition: Partition): PermGrp[G] =
    Grp.fromGeneratorsAndOrder(generators[G](partition), order(partition))

}
