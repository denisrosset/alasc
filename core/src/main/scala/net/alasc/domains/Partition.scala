package net.alasc.domains

import scala.collection.{BitSet, SortedSet}
import scala.collection.mutable
import scala.collection.immutable

import spire.algebra.PartialOrder
import spire.algebra.lattice._
import spire.util.Opt

import net.alasc.algebra.PermutationAction
import net.alasc.syntax.permutationAction._

import algos._

/** Represents an union of disjoint subsets. The subsets are internally
  * stored in an array, sorted by their minimal element.
  */
abstract class Partition extends InDomain {

  type InAnother[D0 <: Domain with Singleton] = Partition.In[D0]

  /** Linked list encoded in array; `linkArray(i)` gives the next element in the
    * block of `i`, with `linkArray(i) > i`, or `-1` if at the end of the block. */
  val linkArray: Array[Int]

  /** Index of the block in which `i` is contained */
  val indexArray: Array[Int]

  /** Minimal element of the `k`-th block */
  val startArray: Array[Int]

  /** Returns the minimal representative of the block in which `k` is contained.
    * Must have `0 <= k < size`.
    */
  def representative(k: Int): Int = startArray(indexArray(k))

  override def toString = blocks.map(_.mkString("[", " ", "]")).mkString

  override def hashCode = scala.util.hashing.MurmurHash3.arrayHash(indexArray)

  override def equals(other: Any) = other match {
    case that: Partition => this.indexArray.sameElements(that.indexArray)
    case _ => false
  }

  def size: Int = domain.size

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
    def contains(l: Int) = if (l < domain.size) indexArray(l) == index else false
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
  def inDomain(newDomain: Domain): Opt[Partition.In[newDomain.type]] =
    if (domain eq newDomain)
      Opt(Partition.this.asInstanceOf[Partition.In[newDomain.type]])
    else if (newDomain.size > size)
      Opt(Partition.fromSeq(newDomain)(indexArray ++ (nBlocks until nBlocks + (newDomain.size - size))))
    else { // newDomain.size < size
      for (k <- newDomain.size until size)
        if (blockFor(k).size > 1) return Opt.empty[Partition.In[newDomain.type]]
      Opt(Partition.fromSeq(newDomain)(indexArray.take(newDomain.size)))
    }

}

abstract class PartitionLowPriority {

  implicit val partialOrder: PartialOrder[Partition] = new PartitionPartialOrder

  implicit val lattice: Lattice[Partition] with BoundedJoinSemilattice[Partition] = new PartitionLattice

}

object Partition extends PartitionLowPriority {

  type In[D0 <: Domain with Singleton] = Partition { type D = D0 }

  implicit def boundedLatticeIn[D <: Domain with Singleton]
  (implicit witness: shapeless.Witness.Aux[D]): BoundedLattice[Partition.In[D]] =
    new PartitionBoundedLatticeIn[D](witness.value)

  implicit def partialOrderIn[D <: Domain with Singleton]
  (implicit witness: shapeless.Witness.Aux[D]): PartialOrder[Partition.In[D]] =
    new PartitionPartialOrderIn[D](witness.value)

  val empty: Partition.In[Domain.empty.type] = Partition(Domain.empty)()

  def apply(sets: Set[Int]*): Partition = {
    val domain = Domain((0 /: sets) { case (mx, set) => mx.max(set.max + 1) } )
    Partition(domain)(sets:_*)
  }

  def apply(domain: Domain)(sets: Set[Int]*): Partition.In[domain.type] = {
    val cumSizes = (0 /: sets.map(_.size))(_+_)
    val setOfSets = sets.flatten.toSet
    if (setOfSets.isEmpty) return fromSortedBlocks(domain)(Seq.empty[SortedSet[Int]])
    val minP = setOfSets.min
    val maxP = setOfSets.max
    assert(setOfSets.size == cumSizes)
    assert(cumSizes == maxP - minP + 1)
    val sortedBlocks = sets.map(setToSortedSet(_)).sortBy(_.min)
    fromSortedBlocks(domain)(sortedBlocks)
  }

  def setToSortedSet(set: Set[Int]): SortedSet[Int] = set match {
    case sortedSet: SortedSet[Int] => sortedSet
    case _ =>
      val b = BitSet.newBuilder
      set.foreach { b += _ }
      b.result
  }

  def fromSortedBlocks(domain0: Domain)(blocks: Seq[scala.collection.SortedSet[Int]]): Partition.In[domain0.type] = {
    val n = domain0.size
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
    new Partition {
      type D = domain0.type
      val domain: D = domain0
      val linkArray = la
      val indexArray = ia
      val startArray = sa
    }
  }

  def fromPermutation[P: PermutationAction](domain: Domain)(p: P): Partition.In[domain.type] = {
    val rem = mutable.BitSet.empty ++= (0 until domain.size)
    var blocks = mutable.ArrayBuffer.empty[immutable.BitSet]
    while (rem.nonEmpty) {
      val m = rem.min
      val orbit = immutable.BitSet.empty ++ p.orbit(m)
      blocks += orbit
      rem --= orbit
    }
    fromSortedBlocks(domain)(blocks)
  }

  def fromSeq(seq: Seq[Any]): Partition = fromSeq(Domain(seq.size))(seq)

  def fromSeq(domain: Domain)(seq: Seq[Any]): Partition.In[domain.type] = {
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
    fromSortedBlocks(domain)(blocks)
  }

}

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

  def zero: Partition = Partition.fromSortedBlocks(Domain(0))(Seq.empty)

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
      forest.partition(x.domain)
    }

  // refinement
  def meet(x: Partition, y: Partition): Partition =
    if (x.size > y.size) meet(y, x) else {
      val refinement = PartitionRefinement(x)
      y.blocks.foreach( block => refinement.refine(block.filter(_ < x.size)) )
      refinement.partition(x.domain)
    }

}
