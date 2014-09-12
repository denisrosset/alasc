package net.alasc.math
package bsgs
package algorithms

import scala.annotation.tailrec

import scala.collection.immutable.BitSet
import scala.collection.immutable
import scala.collection.mutable

import spire.algebra.Order
import spire.syntax.groupAction._
import spire.syntax.group._

import net.alasc.algebra.{FaithfulPermutationAction, Subgroup}
import net.alasc.syntax.check._
import net.alasc.util._

case class BaseGuidePartition(partition: Domain#Partition) extends BaseGuide {
  val blocks = partition.sizeIncreasing
  def fullBase = blocks.flatMap(identity)
  def iterator = new Iter(mutable.BitSet.empty,
    debox.Buffer.fromIterable(blocks.map(block => mutable.BitSet.empty ++= block)),
    debox.Buffer.fromIterable(blocks.map(_.size)))

  final class Iter(val currentBlock: mutable.BitSet, val remainingBlocks: debox.Buffer[mutable.BitSet], val remainingBlockSizes: debox.Buffer[Int]) extends BaseGuideIterator {
    def hasNext = currentBlock.nonEmpty || remainingBlocks.nonEmpty

    def next(beta: Int, easyPoints: collection.Set[Int], isFixed: Int => Boolean): Int =
      if (currentBlock.isEmpty) {
        if (remainingBlocks.isEmpty) beta else {
          // we have to find a new block for the base change, with two constraints:
          // - the block contains a point that is not fixed by the group
          // - (nice to have) the block contains an easy point
          @tailrec def findPointAndBlockIndex(lastIndex: Int, index: Int, nonFixed: OptionTuple2NN): OptionTuple2NN =
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
                  easyNonFixed
                else
                  findPointAndBlockIndex(lastIndex - 1, index - 1, newNonFixed)
              } else {
                if (easyNonFixed.nonEmpty)
                  easyNonFixed
                else
                  findPointAndBlockIndex(index, index - 1, newNonFixed)
              }
            } else nonFixed

          findPointAndBlockIndex(remainingBlocks.length - 1, remainingBlocks.length - 1, NoneTuple2NN) match {
            case OptionTuple2NN(point, blockIndex) =>
              currentBlock ++= remainingBlocks(blockIndex)
              remainingBlocks.remove(blockIndex)
              remainingBlockSizes.remove(blockIndex)
              currentBlock -= point
              point
            case _ => beta
          }
        }
      } else {
        var nonFixed = NNNone
        val toRemove = mutable.BitSet.empty
        currentBlock.foreach { k =>
          if (isFixed(k))
            toRemove += k
          else if (easyPoints.contains(k)) {
            currentBlock --= toRemove
            currentBlock -= k
            return k
          }
          else if (nonFixed.isEmpty)
            nonFixed = NNSome(k)
        }
        currentBlock --= toRemove
        nonFixed match {
          case NNOption(k) =>
            currentBlock -= k
            k
          case _ =>
            next(beta, easyPoints, isFixed)
        }
      }
  }
}
