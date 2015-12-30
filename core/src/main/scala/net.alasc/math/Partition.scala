package net.alasc.math

import scala.annotation.tailrec

import scala.collection.{BitSet, SortedSet}
import scala.collection.mutable
import scala.collection.immutable
import scala.reflect.ClassTag

import spire.algebra.{Eq, PartialOrder}
import spire.algebra.lattice._
import spire.syntax.eq._
import spire.syntax.partialOrder._
import spire.syntax.lattice._
import spire.util.Opt

import net.alasc.algebra.{PermutationAction}
import net.alasc.syntax.permutationAction._
import net.alasc.util._
import partition._

final class PartitionBuilder[D <: Domain with Singleton](val domain: D) extends AnyVal {

  def apply(sets: Set[Int]*): Partition[D] = {
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

  def fromSortedBlocks(blocks: Seq[scala.collection.SortedSet[Int]]) = {
    val n = domain.size
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
    new Partition[D](domain, la, ia, sa)
  }

  def fromPermutation[P: PermutationAction](p: P): Partition[D] = {
    val rem = mutable.BitSet.empty ++= (0 until domain.size)
    var blocks = mutable.ArrayBuffer.empty[immutable.BitSet]
    while (rem.nonEmpty) {
      val m = rem.min
      val orbit = immutable.BitSet.empty ++ p.orbit(m)
      blocks += orbit
      rem --= orbit
    }
    fromSortedBlocks(blocks)
  }

  def fromSeq(seq: Seq[Any]): Partition[D] = {
    require(seq.size == domain.size)
    val blocks = mutable.ArrayBuffer.empty[mutable.BitSet]
    val blockMap = mutable.HashMap.empty[Any, Int]
    seq.indices.foreach { i =>
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

/** Represents an union of disjoint subsets. The subsets are internally
  * stored in an array, sorted by their minimal element.
  * 
  * @param linkArray  Linked list encoded in array; `linkArray(i)` gives the next element in the
  *                   block of `i`, with `linkArray(i) > i`, or `-1` if at the end of the block
  * @param indexArray Index of the block in which `i` is contained
  * @param startArray Minimal element of the `k`-th block
  */
final class Partition[D <: Domain with Singleton](
    val domain: D,
    val linkArray: Array[Int],
    val indexArray: Array[Int],
    val startArray: Array[Int]) extends InDomain[D] {

  /** Returns the minimal representative of the block in which `k` is contained.
    * Must have `0 <= k < size`.
    */
  @inline def representative(k: Int): Int = startArray(indexArray(k))

  override def toString = blocks.map(_.mkString("[", " ", "]")).mkString

  override def hashCode = scala.util.hashing.MurmurHash3.arrayHash(indexArray)

  override def equals(other: Any) = other match {
    case that: Partition[_] => this.indexArray.sameElements(that.indexArray)
    case _ => false
  }

  @inline def size: Int = domain.size

  /** Returns the number of blocks. */
  @inline def nBlocks = startArray.length

  /** Returns the sequence of blocks, the block size increasing. */
  def sizeIncreasing: Seq[Set[Int]] = blocks.sortBy(b => (b.size, b.min))

  def fixingGroupOrder: BigInt = blocks.map(_.symGroupOrder).reduce(_*_)
  def fixingGroupGenerators: Iterable[Perm] = blocks.flatMap(_.symGroupGenerators)
  def fixingGroup: Grp[Perm] = Grp.fromGeneratorsAndOrder(fixingGroupGenerators, fixingGroupOrder)

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
    def contains(l: Int) = if (l < domain.size) indexArray(l) == index else false
    def symGroupOrder = Sym.order(size)
    def symGroupGenerators: Iterable[Perm] =
      if (size <= 1) Iterable.empty else {
        val builder = mutable.ArrayBuilder.make[Perm]
        builder.sizeHint(size - 1)
        var k = start
        while (hasNext(k)) {
          val k1 = next(k)
          builder += Perm(k, k1)
          k = k1
        }
        builder.result
      }
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
    * added or removed, is composed of single points. Returns RefNone if the resizing is not possible,
    * e.g. because the removed last points are not single in a partition.
    */
  def inDomain(newDomain: Domain): Opt[Partition[newDomain.type]] =
    if (domain eq newDomain)
      Opt(Partition.this.asInstanceOf[Partition[newDomain.type]])
    else if (newDomain.size > size)
      Opt(newDomain.Partition.fromSeq(indexArray ++ (nBlocks until nBlocks + (newDomain.size - size))))
    else { // newDomain.size < size
      for (k <- newDomain.size until size)
        if (blockFor(k).size > 1) return Opt.empty[Partition[newDomain.type]]
      Opt(newDomain.Partition.fromSeq(indexArray.take(newDomain.size)))
    }

}

class PartitionPartialOrder[D <: Domain with Singleton](domain: D) extends PartialOrder[Partition[D]] {

  def isRefinementOf(x: Partition[D], y: Partition[D]): Boolean = {
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

  def partialCompare(x: Partition[D], y: Partition[D]): Double =
    (x.nBlocks - y.nBlocks).signum match {
      case 0 =>
        if (x.blocks.sameElements(y.blocks)) 0.0 else Double.NaN
      case 1 => // x can be a refinement of y
        if (isRefinementOf(x, y)) -1.0 else Double.NaN
      case _ => // y can be a refinement of x
        if (isRefinementOf(y, x)) 1.0 else Double.NaN
    }

}

class PartitionBoundedLattice[D <: Domain with Singleton](domain: D) extends BoundedLattice[Partition[D]] {

  def zero: Partition[D] = (domain: D).Partition.fromSortedBlocks(Seq.tabulate(domain.size)( i => immutable.BitSet(i) ))

  def one: Partition[D] = (domain: D).Partition.fromSortedBlocks(Seq(immutable.BitSet.empty ++ (0 until domain.size)))

  // union
  def join(x: Partition[D], y: Partition[D]): Partition[D] = {
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
  def meet(x: Partition[D], y: Partition[D]): Partition[D] = {
    assert(x.domain eq y.domain)
    val refinement = PartitionRefinement(x)
    y.blocks.foreach( refinement.refine(_) )
    refinement.partition(domain: D)
  }

}

object Partition {

  implicit def PartitionBoundedLattice[D <: Domain with Singleton](implicit witness: shapeless.Witness.Aux[D]): BoundedLattice[Partition[D]] = new PartitionBoundedLattice[D](witness.value)

  implicit def PartitionPartialOrder[D <: Domain with Singleton](implicit witness: shapeless.Witness.Aux[D]): PartialOrder[Partition[D]] = new PartitionPartialOrder[D](witness.value)

}
