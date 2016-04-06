package net.alasc.prep.bsgs

import net.alasc.domains.{Domain, Partition}
import net.alasc.util._

import scala.annotation.tailrec

case class BaseGuidePartition(partition: Partition) extends BaseGuide {

  val blocks = partition.sizeIncreasing.reverse
  def fullBase = blocks.flatMap(identity)
  def iterator = new Iter(MutableBitSet.empty,
    metal.mutable.Buffer.fromIterable(blocks.map(block => MutableBitSet.empty ++= block)),
    metal.mutable.Buffer.fromIterable(blocks.map(_.size)))

  final class Iter(val currentBlock: MutableBitSet, val remainingBlocks: metal.mutable.Buffer[MutableBitSet], val remainingBlockSizes: metal.mutable.Buffer[Int]) extends BaseGuideIterator {

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
              val toRemove = MutableBitSet.empty
              block.foreachFast { k =>
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
        val toRemove = MutableBitSet.empty
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
