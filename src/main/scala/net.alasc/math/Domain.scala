package net.alasc.math

import scala.annotation.tailrec

import scala.collection.BitSet
import scala.collection.mutable
import scala.collection.immutable
import scala.reflect.ClassTag

import spire.algebra.{Eq, PartialOrder}
import spire.syntax.eq._
import spire.syntax.partialOrder._

import net.alasc.algebra.{BoundedLattice, BoundedBelowLattice, PermutationAction}
import net.alasc.syntax.permutationAction._
import net.alasc.syntax.lattice._
import net.alasc.util._
import partition._

final class Domain private (val size: Int) { domainSelf =>
  override def toString = s"Domain($size)"
  /** Represents an union of disjoint subsets. The subsets are internally
    * stored in an array, sorted by their minimal element.
    * 
    * @param linkArray  Linked list encoded in array; `linkArray(i)` gives the next element in the
    *                   block of `i`, with `linkArray(i) > i`, or `-1` if at the end of the block
    * @param indexArray Index of the block in which `i` is contained
    * @param startArray Minimal element of the `k`-th block
    */
  final class Partition(linkArray: Array[Int],
    indexArray: Array[Int],
    startArray: Array[Int]) {

    /** Returns the minimal representative of the block in which `k` is contained.
      * Must have `0 <= k < size`.
      */
    def representative(k: Int): Int = startArray(indexArray(k))

    override def toString = blocks.map(_.mkString("[", " ", "]")).mkString
    override def hashCode = scala.util.hashing.MurmurHash3.arrayHash(indexArray)

    def domain: Domain = Domain.this
    def size: Int = Domain.this.size

    /** Returns the number of blocks. */
    def numBlocks = startArray.length

    /** Returns the sequence of blocks, the block size increasing. */
    def sizeIncreasing: Seq[Set[Int]] = blocks.sortBy(b => (b.size, b.min))

    def fixingGroupOrder: BigInt = blocks.map(_.symGroupOrder).reduce(_*_)
    def fixingGroupGenerators: Iterable[Perm] = blocks.flatMap(_.symGroupGenerators)
    def fixingGroup: Grp[Perm] = Grp.fromGeneratorsAndOrder(fixingGroupGenerators, fixingGroupOrder)

    /** Describes the set of points contained in a block. */
    class Block(index: Int) extends FastSetInt {
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
      def contains(l: Int) = if (l < Domain.this.size) indexArray(l) == index else false
      def symGroupOrder = factorial(size)
      def symGroupGenerators =
        if (size <= 1) Iterable.empty else (this zip tail) map { case (x,y) => Perm(x,y) }
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
    def inDomain(newDomain: Domain): RefOption[newDomain.Partition] =
      if (newDomain.size == size)
        RefSome(Partition.this.asInstanceOf[newDomain.Partition])
      else if (newDomain.size > size)
        RefSome(newDomain.Partition.fromSeq(indexArray ++ (numBlocks until numBlocks + (newDomain.size - size))))
      else { // newDomain.size < size
        for (k <- newDomain.size until size)
          if (blockFor(k).size > 1) return RefNone
        RefSome(newDomain.Partition.fromSeq(indexArray.take(newDomain.size)))
      }
  }
  object Partition {
    def fromPermutation[P: PermutationAction](p: P) = {
      val rem = mutable.BitSet.empty ++= (0 until size)
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
      require(seq.size == size)
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
    def unapply(gen: Domain#Partition): RefOption[Partition] =
      if (gen.domain eq Domain.this) RefSome(gen.asInstanceOf[Partition]) else RefNone
    def fromSortedBlocks(blocks: Seq[scala.collection.Set[Int]]) = {
      val n = Domain.this.size
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
    implicit object Algebra extends BoundedLattice[Partition] {
      def zero = Partition.fromSortedBlocks(Seq.tabulate(size)( i => immutable.BitSet(i) ))
      def one = Partition.fromSortedBlocks(Seq(immutable.BitSet.empty ++ (0 until size)))
      // union
      def join(x: Partition, y: Partition): Partition = {
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
        forest.partition(Domain.this)
      }

      // refinement
      def meet(x: Partition, y: Partition): Partition = {
        assert(x.domain eq y.domain)
        val refinement = PartitionRefinement(x)
        y.blocks.foreach( refinement.refine(_) )
        refinement.partition(Domain.this)
      }

      def isRefinementOf(x: Partition, y: Partition): Boolean = {
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

      def partialCompare(x: Partition, y: Partition): Double = {
        assert(x.domain eq y.domain)

        (x.numBlocks - y.numBlocks).signum match {
          case 0 =>
            if (x.blocks.sameElements(y.blocks)) 0.0 else Double.NaN // TODO: can be optimized
          case 1 => // x can be a refinement of y
            if (isRefinementOf(x, y)) -1.0 else Double.NaN
          case _ => // y can be a refinement of x
            if (isRefinementOf(y, x)) 1.0 else Double.NaN
        }
      }
    }
  }
}

object Domain extends UniquenessCache[Int, Domain] {
  protected def valueFromKey(size: Int): Domain = new Domain(size)
  protected def keyFromValue(domain: Domain): Option[Int] = Some(domain.size)
}

object Partition {
  def fromSeq(seq: Seq[Any]): Domain#Partition = Domain(seq.size).Partition.fromSeq(seq)
}
