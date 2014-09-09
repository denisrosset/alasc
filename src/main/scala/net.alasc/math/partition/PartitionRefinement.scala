package net.alasc.math
package partition

import scala.annotation.tailrec

import scala.collection.BitSet
import scala.collection.mutable
import scala.collection.immutable

import spire.algebra.{PartialOrder, Order}

import net.alasc.util._

class PartitionRefinement(val blocks: debox.Buffer[mutable.BitSet], val size: Int) {
  def refine(x: Set[Int]): Unit = {
    val xBitSet = immutable.BitSet.empty ++ x
    val oldLength = blocks.length
    var i = 0
    while (i < oldLength) {
      val intersection = blocks(i) & xBitSet
      if (!intersection.isEmpty && !(intersection.size == blocks(i).size)) {
        blocks(i) &~= intersection
        blocks += intersection
      }
      i += 1
    }
    blocks.sort(Order.from[mutable.BitSet]( (x, y) => (x.min - y.min).signum ))
  }
  def partition(domain: Domain): domain.Partition = domain.Partition.fromArray(blocks.iterator.map(_.toImmutable).toArray)
}

object PartitionRefinement {
  def apply(size: Int) = new PartitionRefinement(debox.Buffer(mutable.BitSet.empty ++= (0 until size)), size)
  def apply(partition: Domain#Partition) = {
    val blocks = debox.Buffer.empty[mutable.BitSet]
    partition.blocks.foreach { block => blocks += (mutable.BitSet.empty ++= block) }
    new PartitionRefinement(blocks, partition.size)
  }
}
