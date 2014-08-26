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

// TODO: sort in reverse order so we just have to pop the last block

final class PartitionGuide(val currentBlock: mutable.BitSet, val remainingBlocks: debox.Buffer[mutable.BitSet], val remainingBlockSizes: debox.Buffer[Int]) extends BaseGuide {
  def isEmpty = currentBlock.isEmpty && remainingBlocks.isEmpty
  def basePoint(easyPoints: collection.Set[Int], isFixed: Int => Boolean): Int = {
    require(easyPoints.nonEmpty)
    if (currentBlock.isEmpty) {
      // we have to find a new block for the base change, with two constraints:
      // - the block contains a point that is not fixed by the group
      // - (nice to have) the block contains an easy point
      @tailrec def findPointAndBlockIndex(lastIndex: Int, index: Int, nonFixed: OptionTuple2NN): Tuple2Int =
        if (index >= 0 && remainingBlockSizes(lastIndex) == remainingBlockSizes(index)) {
          var newNonFixed = nonFixed
          var easyNonFixed = NoneTuple2NN
          val block = remainingBlocks(index)
          val toRemove = mutable.BitSet.empty
          block.foreach { k =>
            if (isFixed(k))
              toRemove += k
            else if (easyPoints.contains(k))
              easyNonFixed = SomeTuple2NN(k, index)
            else if (newNonFixed.isEmpty)
              newNonFixed = SomeTuple2NN(k, index)
          }
          block --= toRemove
          if (block.isEmpty) {
            remainingBlocks.remove(index)
            remainingBlockSizes.remove(index)
            if (easyNonFixed.nonEmpty)
              easyNonFixed.get
            else
              findPointAndBlockIndex(lastIndex - 1, index - 1, newNonFixed)
          } else {
            if (easyNonFixed.nonEmpty)
              easyNonFixed.get
            else
              findPointAndBlockIndex(index, index - 1, newNonFixed)
          }
        } else nonFixed match {
          case OptionTuple2NN(point, blockIndex) => Tuple2Int(point, blockIndex)
          case _ => sys.error("All points are fixed, should not happen.")
        }
      val Tuple2Int(point, blockIndex) = findPointAndBlockIndex(remainingBlocks.length - 1, remainingBlocks.length - 1, NoneTuple2NN)
      currentBlock ++= remainingBlocks(blockIndex)
      remainingBlocks.remove(blockIndex)
      remainingBlockSizes.remove(blockIndex)
      return point
    } else {
      var nonFixed = NNNone
      val toRemove = mutable.BitSet.empty
      currentBlock.foreach { k =>
        if (isFixed(k))
          toRemove += k
        else if (easyPoints.contains(k)) {
          currentBlock --= toRemove
          return k
        }
        else if (nonFixed.isEmpty)
          nonFixed = NNSome(k)
      }
      currentBlock --= toRemove
      if (nonFixed.nonEmpty)
        return nonFixed.get
    }
    basePoint(easyPoints, isFixed)
  }
  
  def moveToNext[P](chosenPoint: Int) = {
    assert(currentBlock.contains(chosenPoint))
    currentBlock -= chosenPoint
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
    new Partition(sets.flatten.max + 1, sets.toSeq.map(set => BitSet.empty ++ set).sortBy(block => -block.size))
  def fromSeq(seq: Seq[Any]): Partition = {
    val map = mutable.HashMap.empty[Any, mutable.BitSet]
    seq.indices.foreach { i => map.getOrElseUpdate(seq(i), mutable.BitSet.empty) += i }
    new Partition(seq.length, map.values.toSeq.sortBy(block => -block.size))
  }
}
