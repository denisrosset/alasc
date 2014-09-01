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

import net.alasc.algebra.{FaithfulPermutationAction, Subgroup}
import net.alasc.syntax.check._
import net.alasc.util._

/** Partition of n elements from the set {0 ... n - 1}, with n >= 1. */
class Partition(val blocks: Seq[BitSet]) {
  lazy val n = blocks.map(_.max).max + 1
  lazy val blockIndex: Array[Int] = {
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
  def guide = PartitionGuide(this)
}

object Partition {
  def fromSets(sets: Iterable[Iterable[Int]]): Partition =
    new Partition(sets.toSeq.map(set => BitSet.empty ++ set).sortBy(block => -block.size))
  def fromSeq(seq: Seq[Any]): Partition = {
    val map = mutable.HashMap.empty[Any, mutable.BitSet]
    seq.indices.foreach { i => map.getOrElseUpdate(seq(i), mutable.BitSet.empty) += i }
    new Partition(map.values.toSeq.sortBy(block => -block.size))
  }
}
