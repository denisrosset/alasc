package net.alasc.math
package bsgs
package algorithms

import scala.annotation.tailrec

import scala.collection.BitSet
import scala.collection.immutable
import scala.collection.mutable

import spire.algebra.Order
import spire.syntax.groupAction._
import spire.syntax.group._

import net.alasc.algebra.{PermutationAction, Subgroup}
import net.alasc.syntax.check._
import net.alasc.util._

class PartitionGuide(val currentBlock: mutable.BitSet, val remainingBlocks: debox.Buffer[mutable.BitSet], val remainingBlockSizes: debox.Buffer[Int]) extends BaseGuide {
  def isEmpty = currentBlock.isEmpty && remainingBlocks.isEmpty
  def basePoint(easyPoints: collection.Set[Int]): Int =
    if (currentBlock.isEmpty) {
      if (easyPoints.isEmpty) {
        currentBlock ++= remainingBlocks(0)
        remainingBlocks.remove(0)
        remainingBlockSizes.remove(0)
        currentBlock.head
      } else {
        val n = remainingBlocks.length
        if (n == 0) {
          println("Warning")
          return easyPoints.head
        }
        @tailrec def findPointAndBlockIndex(lastIndex: Int, index: Int): Tuple2Int =
          if (index < n) {
            if (remainingBlockSizes(index) != remainingBlockSizes(lastIndex))
              Tuple2Int(remainingBlocks(lastIndex).head, lastIndex)
            else
              easyPoints.find(k => remainingBlocks(index).contains(k)) match {
                case Some(k) => Tuple2Int(k, index)
                case None => findPointAndBlockIndex(index, index + 1)
              }
          } else Tuple2Int(remainingBlocks(0).head, 0)
        val Tuple2Int(point, blockIndex) = findPointAndBlockIndex(0, 0)
        currentBlock ++= remainingBlocks(blockIndex)
        remainingBlocks.remove(blockIndex)
        remainingBlockSizes.remove(blockIndex)
        point
      }
    } else {
      easyPoints.find(k => currentBlock.contains(k)) match {
        case Some(k) => k
        case None => currentBlock.head
      }
    }
  
  def moveToNext[P](chosenPoint: Int, isFixed: Int => Boolean) =
    if (currentBlock.nonEmpty || remainingBlocks.nonEmpty) {
      assert(currentBlock.contains(chosenPoint))
      currentBlock -= chosenPoint
      val toRemove = mutable.BitSet.empty
      currentBlock.foreach { k => if (isFixed(k)) toRemove += k }
      currentBlock --= toRemove
      @tailrec def removeFixedFromBlocks(i: Int, n: Int, toRemove: mutable.BitSet): Unit =
        if (i < n) {
          toRemove.clear
          remainingBlocks(i).foreach { k => if (isFixed(k)) toRemove += k }
          remainingBlocks(i) --= toRemove
          if (remainingBlocks(i).isEmpty) {
            remainingBlocks.remove(i)
            remainingBlockSizes.remove(i)
            removeFixedFromBlocks(i, n - 1, toRemove)
          } else
            removeFixedFromBlocks(i + 1, n, toRemove)
        }
      removeFixedFromBlocks(0, remainingBlocks.length, toRemove) // we reuse toRemove
    }
}

/** Partition of n elements from the set {0 ... n - 1}. */
class Partition(val n: Int, val blocks: Seq[BitSet]) {
  lazy val blockIndex: Array[Int] = {
    val n = blocks.map(_.max).max + 1
    assert(blocks.forall { _.forall { _ < n } })
    val res = Array.fill(n)(-1)
    var i = 0
    while (i < blocks.length) {
      blocks(i).foreach { k => res(k) = i }
      i += 1
    }
    res
  }
  def blockSize(k: Int) = blocks(blockIndex(k)).size
  override def toString = blocks.map(_.mkString("[", " ", "]")).mkString
  def guide: PartitionGuide =
    new PartitionGuide(mutable.BitSet.empty,
      debox.Buffer.fromIterable(blocks.map(bitset => mutable.BitSet.fromBitMaskNoCopy(bitset.toBitMask))),
      debox.Buffer.fromIterable(blocks.map(_.size)))
}

object Partition {
  def fromSets(sets: Iterable[Iterable[Int]]): Partition =
    new Partition(sets.flatten.max + 1, sets.toSeq.map(set => BitSet.empty ++ set).sortBy(_.size))
  def fromSeq(seq: Seq[Any]): Partition = {
    val map = mutable.HashMap.empty[Any, mutable.BitSet]
    seq.indices.foreach { i => map.getOrElseUpdate(seq(i), mutable.BitSet.empty) += i }
    new Partition(seq.length, map.values.toSeq.sortBy(_.size))
  }
}
