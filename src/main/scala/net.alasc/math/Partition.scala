package net.alasc.math

import scala.annotation.tailrec

import scala.collection.immutable
import scala.collection.mutable

import spire.algebra.{Eq, Order}
import spire.syntax.eq._
import spire.syntax.groupAction._
import spire.syntax.group._

import net.alasc.algebra.{FaithfulPermutationAction, Subgroup}
import net.alasc.syntax.check._
import net.alasc.util._

/** Partition of the domain {0, ... size - 1}, with size >= 1. */
trait Partition {
  override def toString = blocks.map(_.mkString("[", " ", "]")).mkString
  /** Returns the size of the partition, >= 1. */
  def size: Int
  /** Returns an iterable of the partition blocks. */
  def blocks: Iterable[Set[Int]]
  /** Returns the block containing `k`. */
  def blockFor(k: Int): Set[Int]
  /** Returns a representative of the block in which `k` belongs. */
  def representative(k: Int): Int
  def sizeIncreasing: OrderedPartition = OrderedPartition(size, blocks.toSeq.sortBy(b => (b.size, b.min)))
}

case class OrderedPartition(size: Int, blocks: Seq[Set[Int]]) extends Partition {
  lazy val reps: Array[Int] = {
    val res = new Array[Int](size)
    blocks.foreach { block =>
      val rep = block.min
      block.foreach { k => res(k) = rep }
    }
    res
  }
  def blockFor(k: Int): Set[Int] = {
    require(0 <= k && k < size)
    blocks.find(_.contains(k)).get
  }
  def representative(k: Int) = reps(k)
}

object Partition {
  def fromSets(sets: Iterable[Iterable[Int]]): Partition = {
    val size = sets.map(_.max).max + 1
    OrderedPartition(size, sets.map(_.toSet).toSeq)
  }
  def fromSeqEq[A](seq: Seq[A])(implicit eq: Eq[A]): Partition = {
    val size = seq.size
    val blocks = mutable.ArrayBuffer.empty[mutable.BitSet]
    var i = 0
    while (i < size) {
      var j = 0
      var found = false
      while (j < blocks.length && !found) {
        if (seq(blocks(j).head) === seq(i)) {
          blocks(j) += i
          found = true
        }
        j += 1
      }
      if (!found)
        blocks += mutable.BitSet(i)
      i += 1
    }
    OrderedPartition(size, blocks.result.map(_.toImmutable))
  }

  def fromSeqHashCode(seq: Seq[Any]): Partition = {
    val map = mutable.HashMap.empty[Any, mutable.BitSet]
    seq.indices.foreach { i => map.getOrElseUpdate(seq(i), mutable.BitSet.empty) += i }
    OrderedPartition(seq.size, map.values.map(_.toImmutable).toSeq)
  }
}
