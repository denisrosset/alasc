package net.alasc.partitions

import scala.collection.{BitSet, SortedSet}
import scala.collection.mutable
import scala.collection.immutable

import spire.algebra.PartialOrder
import spire.algebra.lattice._
import spire.syntax.cfor._
import spire.util.Opt

import net.alasc.algebra.PermutationAction
import net.alasc.syntax.permutationAction._

import algos._

/** Represents an union of disjoint subsets. The subsets are internally
  * stored in an array, sorted by their minimal element.
  *
  * @param linkArray Linked list encoded in array; `linkArray(i)` gives the next element in the
  *                  block of `i`, with `linkArray(i) > i`, or `-1` if at the end of the block.
  *
  * @param indexArray Index of the block in which `i` is contained
  *
  * @param startArray Minimal element of the `k`-th block
  */
class Partition(val linkArray: Array[Int],
                val indexArray: Array[Int],
                val startArray: Array[Int]) {

  /** Returns the minimal representative of the block in which `k` is contained.
    * Must have `0 <= k`. For `k >= size` returns size.
    */
  def representative(k: Int): Int = if (k < size) startArray(indexArray(k)) else size

  override def toString = blocks.map(_.mkString("[", " ", "]")).mkString

  override def hashCode = scala.util.hashing.MurmurHash3.arrayHash(indexArray)

  override def equals(other: Any) = other match {
    case that: Partition => this.indexArray.sameElements(that.indexArray)
    case _ => false
  }

  def size: Int = indexArray.length

  /** Returns the number of blocks. */
  def nBlocks = startArray.length

  /** Returns the sequence of blocks, the block size increasing. */
  def sizeIncreasing: Seq[Set[Int]] = blocks.sortBy(b => (b.size, b.min))

  /** Describes the set of points contained in a block. */
  class Block(index: Int) extends Set[Int] { self =>
    def +(i: Int) = iterator.toSet + i
    def -(i: Int) = iterator.toSet - i
    def iterator = new Iterator[Int] {
      private[this] var current = start
      def hasNext = current != -1
      def next: Int = {
        val res = current
        if (self.hasNext(current))
          current = self.next(current)
        else
          current = -1
        res
      }
    }
    override def min[B >: Int](implicit cmp: Ordering[B]): Int = startArray(index)
    override def isEmpty = false
    override def nonEmpty = true
    def start = startArray(index)
    def hasNext(l: Int) = linkArray(l) != -1
    def next(l: Int) = {
      val n = linkArray(l)
      if (n == -1) Iterator.empty.next
      n
    }
    def contains(l: Int) = if (l < Partition.this.size) indexArray(l) == index else false
  }

  /** Returns the block in which `k` is contained. Must have `0 <= k < size`. */
  def blockFor(k: Int): Block = new Block(indexArray(k))

  /** Returns the index of the block in which `k` is contained. */
  def blockIndex(k: Int): Int = indexArray(k)

  /** Returns the blocks in the partition, ordered by their minimal element. */
  def blocks: Seq[Block] = new IndexedSeq[Block] {
    def length = startArray.length
    def apply(index: Int) = new Block(index)
  }

  /** Returns the blocks that intersect the set `points`. */
  def blocksFor(points: Set[Int]): Seq[Block] = {
    val buf = mutable.ArrayBuffer.empty[Block]
    var remaining = mutable.BitSet.empty ++= points
    while (remaining.nonEmpty && remaining.min < size) {
      val m = remaining.min
      val b = blockFor(m)
      buf += b
      remaining --= b
    }
    buf.result
  }

  /** Returns the partition in a domain of possibly a different size. The resized part, to be
    * added or removed, is composed of single points. Returns Opt.empty if the resizing is not possible,
    * e.g. because the removed last points are not single in a partition.
    */
  def resize(newSize: Int): Opt[Partition] =
    if (size == newSize) Opt(this)
    else if (newSize > size)
      Opt(Partition.fromSeq(indexArray ++ (nBlocks until nBlocks + (newSize - size))))
    else { // newSize < size
      cforRange(newSize until size) { k =>
        if (blockFor(k).size > 1) return Opt.empty[Partition]
      }
      Opt(Partition.fromSeq(indexArray.take(newSize)))
    }

}

abstract class PartitionLowPriority {

  implicit val partialOrder: PartialOrder[Partition] = new PartitionPartialOrder

  implicit val lattice: Lattice[Partition] with BoundedJoinSemilattice[Partition] = new PartitionLattice

}

object Partition extends PartitionLowPriority {

  /*
  implicit def boundedLatticeIn[D <: Domain with Singleton]
  (implicit witness: shapeless.Witness.Aux[D]): BoundedLattice[Partition.In[D]] =
    new PartitionBoundedLatticeIn[D](witness.value)

  implicit def partialOrderIn[D <: Domain with Singleton]
  (implicit witness: shapeless.Witness.Aux[D]): PartialOrder[Partition.In[D]] =
    new PartitionPartialOrderIn[D](witness.value)*/

  def apply(sets: Set[Int]*): Partition = {
    val cumSizes = (0 /: sets.map(_.size))(_+_)
    val setOfSets = sets.flatten.toSet
    if (setOfSets.isEmpty) return fromSortedBlocks(Seq.empty[SortedSet[Int]])
    val minP = setOfSets.min
    val maxP = setOfSets.max
    assert(setOfSets.size == cumSizes)
    assert(cumSizes == maxP - minP + 1)
    val sortedBlocks = sets.map(setToSortedSet(_)).sortBy(_.min)
    fromSortedBlocks(sortedBlocks)
  }

  def setToSortedSet(set: Set[Int]): SortedSet[Int] = set match {
    case sortedSet: SortedSet[Int] => sortedSet
    case _ =>
      val b = BitSet.newBuilder
      set.foreach { b += _ }
      b.result
  }

  def fromSortedBlocks(blocks: Seq[scala.collection.SortedSet[Int]]): Partition = {
    val n = blocks.foldLeft(0)( (mx, set) => mx.max(set.max + 1) )
    val la = new Array[Int](n)
    val ia = new Array[Int](n)
    val sa = new Array[Int](blocks.size)
    blocks.indices.foreach { index =>
      val block = blocks(index)
      sa(index) = block.min
      la(block.max) = -1
      var prev = -1
      block.foreach { k =>
        ia(k) = index
        if (prev != -1)
          la(prev) = k
        prev = k
      }
    }
    new Partition(la, ia, sa)
  }

  def fromPermutation[P: PermutationAction](n: Int, p: P): Partition = {
    val rem = mutable.BitSet.empty ++= (0 until n)
    var blocks = mutable.ArrayBuffer.empty[immutable.BitSet]
    while (rem.nonEmpty) {
      val m = rem.min
      val orbit = immutable.BitSet.empty ++ p.orbit(m)
      blocks += orbit
      rem --= orbit
    }
    fromSortedBlocks(blocks)
  }

  def fromSeq(seq: Seq[Any]): Partition = {
    val blocks = mutable.ArrayBuffer.empty[mutable.BitSet]
    val blockMap = mutable.HashMap.empty[Any, Int]
    cforRange(0 until seq.length) { i =>
      blockMap.get(seq(i)) match {
        case Some(b) => blocks(b) += i
        case None =>
          blockMap += seq(i) -> blocks.length
          blocks += mutable.BitSet(i)
      }
    }
    fromSortedBlocks(blocks)
  }

}

/*
class PartitionPartialOrderIn[D <: Domain with Singleton](val domain: D)
    extends PartialOrder[Partition.In[D]]
    with InDomain.Of[D] {

  def isRefinementOf(x: Partition.In[D], y: Partition.In[D]): Boolean = {
    y.blocks.foreach { block =>
      val mut = block match {
        case bs: BitSet => mutable.BitSet.fromBitMaskNoCopy(bs.toBitMask)
        case _ => mutable.BitSet.empty ++= block
      }
      while (!mut.isEmpty) {
        val m = mut.min
        x.blockFor(m) match {
          case refBlock: BitSet =>
            if (!refBlock.subsetOf(mut))
              return false
            mut --= refBlock
          case other =>
            if (!other.forall(mut.contains(_)))
              return false
            mut --= other
        }
      }
    }
    true
  }

  def partialCompare(x: Partition.In[D], y: Partition.In[D]): Double =
    (x.nBlocks - y.nBlocks).signum match {
      case 0 =>
        if (x.blocks.sameElements(y.blocks)) 0.0 else Double.NaN
      case 1 => // x can be a refinement of y
        if (isRefinementOf(x, y)) -1.0 else Double.NaN
      case _ => // y can be a refinement of x
        if (isRefinementOf(y, x)) 1.0 else Double.NaN
    }

}

class PartitionBoundedLatticeIn[D <: Domain with Singleton](val domain: D)
    extends BoundedLattice[Partition.In[D]]
    with InDomain.Of[D] {

  def zero: Partition.In[D] = Partition.fromSortedBlocks(domain: D)(Seq.tabulate(domain.size)( i => immutable.BitSet(i) ))

  def one: Partition.In[D] = Partition.fromSortedBlocks(domain: D)(Seq(immutable.BitSet.empty ++ (0 until domain.size)))

  // union
  def join(x: Partition.In[D], y: Partition.In[D]): Partition.In[D] = {
    assert(x.domain eq y.domain)
    val forest = DisjointSetForest(x)
    y.blocks.foreach { block =>
      val it = block.iterator
      val el1 = it.next
      while (it.hasNext) {
        val el2 = it.next
        forest.union(el1, el2)
      }
    }
    forest.partition(domain: D)
  }

  // refinement
  def meet(x: Partition.In[D], y: Partition.In[D]): Partition.In[D] = {
    assert(x.domain eq y.domain)
    val refinement = PartitionRefinement(x)
    y.blocks.foreach( refinement.refine(_) )
    refinement.partition(domain: D)
  }

}
*/

final class PartitionPartialOrder extends PartialOrder[Partition] {

  override def eqv(x: Partition, y: Partition): Boolean =
    (x.size == y.size) && (x.blocks.sameElements(y.blocks))

  override def lteqv(x: Partition, y: Partition): Boolean =
    if (x.size <= y.size) {
      y.blocks.foreach { yBlock =>
        val remaining = mutable.BitSet.empty ++= yBlock
        while (!remaining.isEmpty) {
          val m = remaining.min
          if (m >= x.size) {
            remaining.clear
          } else {
            val xBlock = x.blockFor(m)
            if (!xBlock.forall(remaining.contains(_))) return false
            remaining --= xBlock
          }
        }
      }
      true
    } else false

  def partialCompare(x: Partition, y: Partition): Double = {
    val xLTEy = lteqv(x, y)
    val yLTEx = lteqv(y, x)
    if (xLTEy) {
      if (yLTEx) 0.0 else -1.0
    } else {
      if (yLTEx) 1.0 else Double.NaN
    }
  }

}

final class PartitionLattice
    extends Lattice[Partition]
    with BoundedJoinSemilattice[Partition] {

  def zero: Partition = Partition.fromSortedBlocks(Seq.empty)

  // union
  def join(x: Partition, y: Partition): Partition =
    if (x.size < y.size) join(y, x) else {
      val forest = DisjointSetForest(x)
      y.blocks.foreach { block =>
        val it = block.iterator
        val el1 = it.next
        while (it.hasNext) {
          val el2 = it.next
          forest.union(el1, el2)
        }
      }
      forest.toPartition
    }

  // refinement
  def meet(x: Partition, y: Partition): Partition =
    if (x.size > y.size) meet(y, x) else {
      val refinement = PartitionRefinement(x)
      y.blocks.foreach( block => refinement.refine(block.filter(_ < x.size)) )
      refinement.toPartition
    }

}
