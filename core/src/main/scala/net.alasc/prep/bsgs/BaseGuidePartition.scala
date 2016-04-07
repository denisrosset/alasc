package net.alasc.prep.bsgs

import scala.annotation.tailrec

import net.alasc.domains.{Domain, Partition}
import net.alasc.util._

import metal._
import metal.syntax._

case class BaseGuidePartition(partition: Partition) extends BaseGuide {

  val blocks = partition.sizeIncreasing.reverse
  def fullBase = blocks.flatMap(identity)
  def iterator = new Iter(metal.mutable.BitSet.empty,
    metal.mutable.Buffer.fromIterable(blocks.map(block => metal.mutable.BitSet.fromIterable(block))),
    metal.mutable.Buffer.fromIterable(blocks.map(_.size)))

  final class Iter(val currentBlock: metal.mutable.BitSet, val remainingBlocks: metal.mutable.Buffer[metal.mutable.BitSet], val remainingBlockSizes: metal.mutable.Buffer[Int]) extends BaseGuideIterator {

    override def toString = s"Current: $currentBlock, Remaining: $remainingBlocks"
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
              val toRemove = metal.mutable.BitSet.empty
              block.foreach { k =>
                if (isFixed(k))
                  toRemove += k
                else if (easyPoints.contains(k))
                  easyNonFixed = SomeTuple2NN(k, index)
                else if (newNonFixed.isEmpty)
                  newNonFixed = SomeTuple2NN(k, index)
              }
              toRemove.foreach { k => block -= k } // TODO bitset diff
              if (block.isEmpty) {
                remainingBlocks.remove(index)
                remainingBlockSizes.remove(index)
                if (easyNonFixed.nonEmpty) {
                  val OptionTuple2NN(point, blockIndex) = easyNonFixed
                  SomeTuple2NN(point, blockIndex - 1)
                } else newNonFixed match {
                  case OptionTuple2NN(point, blockIndex) =>
                    findPointAndBlockIndex(lastIndex - 1, index - 1, SomeTuple2NN(point, blockIndex - 1))
                  case _ =>
                    findPointAndBlockIndex(lastIndex - 1, index - 1, newNonFixed)
                }
              } else {
                if (easyNonFixed.nonEmpty)
                  easyNonFixed
                else
                  findPointAndBlockIndex(index, index - 1, newNonFixed)
              }
            } else nonFixed

          findPointAndBlockIndex(remainingBlocks.length.toInt - 1, remainingBlocks.length.toInt - 1, NoneTuple2NN) match {
            case OptionTuple2NN(point, blockIndex) =>
              remainingBlocks(blockIndex).foreach { k => currentBlock += k }
              remainingBlocks.remove(blockIndex)
              remainingBlockSizes.remove(blockIndex)
              currentBlock -= point
              point
            case _ => beta
          }
        }
      } else {
        var nonFixed = NNNone
        val toRemove = metal.mutable.BitSet.empty
        currentBlock.foreach { k =>
          if (isFixed(k))
            toRemove += k
          else if (easyPoints.contains(k)) {
            toRemove.foreach { j => currentBlock -= j } // TODO bitset diff
            currentBlock -= k
            return k
          }
          else if (nonFixed.isEmpty)
            nonFixed = NNSome(k)
        }
        toRemove.foreach { j => currentBlock -= j } // TODO bitset diff
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
