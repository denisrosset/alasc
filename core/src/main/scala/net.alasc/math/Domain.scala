package net.alasc.math

import scala.annotation.tailrec

import scala.collection.BitSet
import scala.collection.mutable
import scala.collection.immutable
import scala.reflect.ClassTag

import spire.algebra.{Eq, PartialOrder}
import spire.algebra.lattice._
import spire.syntax.eq._
import spire.syntax.partialOrder._
import spire.syntax.lattice._
import spire.util.Nullbox

import net.alasc.algebra.{PermutationAction}
import net.alasc.syntax.permutationAction._
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
  final class Partition(val linkArray: Array[Int],
    val indexArray: Array[Int],
    val startArray: Array[Int]) {

    /** Returns the minimal representative of the block in which `k` is contained.
      * Must have `0 <= k < size`.
      */
    def representative(k: Int): Int = startArray(indexArray(k))

    override def toString = blocks.map(_.mkString("[", " ", "]")).mkString
    override def hashCode = scala.util.hashing.MurmurHash3.arrayHash(indexArray)
    override def equals(other: Any) = other match {
      case that: Domain#Partition => this.indexArray.sameElements(that.indexArray)
      case _ => false
    }

    val domain: Domain = Domain.this
    def size: Int = Domain.this.size

    /** Returns the number of blocks. */
    def nBlocks = startArray.length

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
    def inDomain(newDomain: Domain): Nullbox[newDomain.Partition] =
      if (newDomain.size == size)
        Nullbox(Partition.this.asInstanceOf[newDomain.Partition])
      else if (newDomain.size > size)
        Nullbox(newDomain.Partition.fromSeq(indexArray ++ (nBlocks until nBlocks + (newDomain.size - size))))
      else { // newDomain.size < size
        for (k <- newDomain.size until size)
          if (blockFor(k).size > 1) return Nullbox.empty[newDomain.Partition]
        Nullbox(newDomain.Partition.fromSeq(indexArray.take(newDomain.size)))
      }
  }
  object Typed {
    def unapply(partition: Domain#Partition): Nullbox[Partition] =
      if (partition.domain eq Domain.this)
        Nullbox(partition.asInstanceOf[Partition])
      else
        Nullbox.empty[Partition]
    def unapply[V](partitionMap: Domain#PartitionMap[V]): Nullbox[PartitionMap[V]] =
      if (partitionMap.domain eq Domain.this)
        Nullbox(partitionMap.asInstanceOf[PartitionMap[V]])
      else
        Nullbox.empty[PartitionMap[V]]
  }
  object Partition {
    def apply(sets: Set[Int]*): Partition = {
      val cumSizes = (0 /: sets.map(_.size))(_+_)
      val setOfSets = sets.flatten.toSet
      if (setOfSets.isEmpty) return fromSortedBlocks(Seq.empty[Set[Int]])
      val minP = setOfSets.min
      val maxP = setOfSets.max
      assert(setOfSets.size == cumSizes)
      assert(cumSizes == maxP - minP + 1)
      val sortedBlocks = sets.sortBy(_.min)
      fromSortedBlocks(sortedBlocks)
    }
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
    trait PartitionBoundedLattice extends BoundedLattice[Partition] {
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
    }
    trait PartitionPartialOrder extends PartialOrder[Partition] {
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

        (x.nBlocks - y.nBlocks).signum match {
          case 0 =>
            if (x.blocks.sameElements(y.blocks)) 0.0 else Double.NaN
          case 1 => // x can be a refinement of y
            if (isRefinementOf(x, y)) -1.0 else Double.NaN
          case _ => // y can be a refinement of x
            if (isRefinementOf(y, x)) 1.0 else Double.NaN
        }
      }
    }
    implicit object Algebra extends PartitionBoundedLattice with PartitionPartialOrder
  }

  /** Describes a partition of the domain with a value `V` associated to each block. */
  final class PartitionMap[V: ClassTag](val partition: Partition, protected val values: Array[V]) {
    assert(partition.domain eq Domain.this)
    /** Returns the size of the underlying domain. */
    def size = Domain.this.size
    val domain = Domain.this
    def blocks: Seq[(Set[Int], V)] = partition.blocks.map(block => (block -> PartitionMap.this(block)))
    override def equals(other: Any) = other match {
      case that: Domain#PartitionMap[_] => (this.partition == that.partition) && (this.partition.blocks.forall(block => this(block) == that(block)))
      case _ => false
    }
    override def hashCode = scala.util.hashing.MurmurHash3.unorderedHash(partition.blocks zip values)
    override def toString = partition.blocks.map( block => block.toString + " -> " + apply(block).toString).mkString("PartitionMap(", ", ", ")")
    def getOrElse(i: Int, defaultValue: => V) =
      if (i < partition.size) apply(i) else defaultValue
    /** Returns the value associated with the block of which `i` is a member. */
    def apply(i: Int): V =
      if (i < 0 || i >= partition.size) throw new IndexOutOfBoundsException(i.toString) else
        values(partition.blockIndex(i))
    /** Returns the value associated with the given `block`. `block` must be exact member of the partition. */
    def apply(block: Set[Int]): V =
      if (block.isEmpty) throw new NoSuchElementException(block.toString) else {
        val m = block.min
        val bi = partition.blockIndex(m)
        if (partition.blocks(bi) != block)
          throw new NoSuchElementException(block.toString)
        else
          values(bi)
      }
    /** Tests if `i` is in a block of the partition. */
    def isDefinedAt(i: Int): Boolean = (i >= 0 && i < partition.size)
    /** Tests if `block` is in the partition. */
    def isDefinedAt(block: Set[Int]): Boolean = block.nonEmpty && partition.blocks(partition.blockIndex(block.min)) == block
    /** Returns a resized partition map, using `defaultValue` when expanding, and testing whether elements can be removed
      * using `removedSatisfy`. Returns `RefNone` if the resizing is not possible. */
    def forDomain(newDomain: Domain, defaultValue: => V, removedSatisfy: V => Boolean): Nullbox[newDomain.PartitionMap[V]] =
      if (newDomain.size == size)
        Nullbox(this.asInstanceOf[newDomain.PartitionMap[V]])
      else if (newDomain.size > size) {
        val newPartition = partition.inDomain(newDomain).get
        val newValues = values ++ Array.fill(newPartition.nBlocks - partition.nBlocks)(defaultValue)
        Nullbox(new newDomain.PartitionMap(newPartition, newValues))
      } else // newSize < partition.size
        partition.inDomain(newDomain) match {
          case Nullbox(newPartition) =>
            if ((newDomain.size until size).forall( i => removedSatisfy(apply(i)) ))
              Nullbox(new newDomain.PartitionMap(newPartition, values.take(values.length - (size - newDomain.size))))
            else
              Nullbox.empty[newDomain.PartitionMap[V]]
          case _ => Nullbox.empty[newDomain.PartitionMap[V]]
        }
  }

  object PartitionMap {
    def apply[V : ClassTag](blocks: (Set[Int], V)*): PartitionMap[V] = {
      val map = blocks.toMap
      val partition = Partition(blocks.map(_._1):_*)
      tabulate(partition)(map(_))
    }
    def fill[V: ClassTag](partition: Partition)(elem: => V): PartitionMap[V] =
      new PartitionMap(partition, Array.fill(partition.nBlocks)(elem))
    def tabulate[V: ClassTag](partition: Partition)(f: Set[Int] => V): PartitionMap[V] =
      new PartitionMap(partition, Array.tabulate(partition.nBlocks)(i => f(partition.blocks(i))))
    trait PartitionMapPartialOrder[V] extends Any with PartialOrder[PartitionMap[V]] {
      implicit def partialOrder: PartialOrder[V]
      override def lteqv(x: PartitionMap[V], y: PartitionMap[V]): Boolean = // x <= y ?
        if (!(x.partition <= y.partition))
          false
        else {
          x.partition.blocks.forall { xBlock =>
            val m = xBlock.min
            x(m) <= y(m)
          }
        }
      override def gteqv(x: PartitionMap[V], y: PartitionMap[V]): Boolean = lteqv(y, x)
      def partialCompare(x: PartitionMap[V], y: PartitionMap[V]): Double =
        if (lteqv(x, y)) {
          if (gteqv(x, y))
            0.0
          else // x <= y but not x >= y
            -1.0
        } else {
          if (gteqv(x, y)) // not x <= y but x >= y
            1.0
          else
            Double.NaN
        }
    }

    trait PartitionMapJoinSemilattice[V] extends Any with JoinSemilattice[PartitionMap[V]] {

      implicit def classTag: ClassTag[V]
      implicit def lattice: JoinSemilattice[V]
      
      def join(x: PartitionMap[V], y: PartitionMap[V]): PartitionMap[V] = {
        val newPartition = x.partition join y.partition
        PartitionMap.tabulate[V](newPartition) { block =>
          val xBlocks = x.partition.blocksFor(block)
          val afterX = (x(xBlocks.head.min) /: xBlocks.tail) {
            case (v, b) => v join x(b.min)
          }

          (afterX /: y.partition.blocksFor(block)) {
            case (v, b) => v join y(b.min)
          }
        }
      }
    }

    trait PartitionMapMeetSemilattice[V] extends Any with MeetSemilattice[PartitionMap[V]] {

      implicit def classTag: ClassTag[V]
      implicit def lattice: MeetSemilattice[V]

      def meet(x: PartitionMap[V], y: PartitionMap[V]): PartitionMap[V] = {
        val newPartition = x.partition meet y.partition
        PartitionMap.tabulate[V](newPartition) { block =>
          val xBlocks = x.partition.blocksFor(block)
          val afterX = (x(xBlocks.head.min) /: xBlocks.tail) {
            case (v, b) => v meet x(b.min)
          }

          (afterX /: y.partition.blocksFor(block)) {
            case (v, b) => v meet y(b.min)
          }
        }
      }
    }

    trait PartitionMapLattice[V] extends Any with Lattice[PartitionMap[V]] with PartitionMapJoinSemilattice[V] with PartitionMapMeetSemilattice[V] {
      implicit def lattice: Lattice[V]
    }

    trait PartitionMapBoundedJoinSemilattice[V] extends Any with BoundedJoinSemilattice[PartitionMap[V]] with PartitionMapJoinSemilattice[V] {
      implicit def lattice: BoundedJoinSemilattice[V]
      def zero = PartitionMap.fill(Partition.Algebra.zero)(lattice.zero)
    }

    trait PartitionMapBoundedMeetSemilattice[V] extends Any with BoundedMeetSemilattice[PartitionMap[V]] with PartitionMapMeetSemilattice[V] {
      implicit def lattice: BoundedMeetSemilattice[V]
      def one = PartitionMap.fill(Partition.Algebra.one)(lattice.one)
    }

    trait PartitionMapBoundedLattice[V] extends Any with BoundedLattice[PartitionMap[V]]
        with PartitionMapBoundedJoinSemilattice[V] with PartitionMapBoundedMeetSemilattice[V] {
      implicit def lattice: BoundedLattice[V]
    }

    /** Induced lattices for partition maps, when the values are themselves member of a lattice. */
    implicit def PartitionMapPartialOrder[V: PartialOrder]: PartialOrder[PartitionMap[V]] = new PartitionMapPartialOrder[V] {
      def partialOrder = implicitly[PartialOrder[V]]
    }
    implicit def PartitionMapJoinSemilattice[V: ClassTag: JoinSemilattice]: JoinSemilattice[PartitionMap[V]] = new PartitionMapJoinSemilattice[V] {
      def classTag = implicitly[ClassTag[V]]
      def lattice = implicitly[JoinSemilattice[V]]
    }
    implicit def PartitionMapMeetSemilattice[V: ClassTag: MeetSemilattice]: MeetSemilattice[PartitionMap[V]] = new PartitionMapMeetSemilattice[V] {
      def classTag = implicitly[ClassTag[V]]
      def lattice = implicitly[MeetSemilattice[V]]
    }
    implicit def PartitionMapLattice[V: ClassTag: Lattice]: Lattice[PartitionMap[V]] = new PartitionMapLattice[V] {
      def classTag = implicitly[ClassTag[V]]
      def lattice = implicitly[Lattice[V]]
    }
    implicit def PartitionMapBoundedJoinSemilattice[V: ClassTag: BoundedJoinSemilattice]: BoundedJoinSemilattice[PartitionMap[V]] = new PartitionMapBoundedJoinSemilattice[V] {
      def classTag = implicitly[ClassTag[V]]
      def lattice = implicitly[BoundedJoinSemilattice[V]]
    }
    implicit def PartitionMapBoundedMeetSemilattice[V: ClassTag: BoundedMeetSemilattice]: BoundedMeetSemilattice[PartitionMap[V]] = new PartitionMapBoundedMeetSemilattice[V] {
      def classTag = implicitly[ClassTag[V]]
      def lattice = implicitly[BoundedMeetSemilattice[V]]
    }
    implicit def PartitionMapBoundedLattice[V: ClassTag: BoundedLattice]: BoundedLattice[PartitionMap[V]] = new PartitionMapBoundedLattice[V] {
      def classTag = implicitly[ClassTag[V]]
      def lattice = implicitly[BoundedLattice[V]]
    }
  }
}

object Domain extends UniquenessCache[Int, Domain] {
  protected def valueFromKey(size: Int): Domain = new Domain(size)
  protected def keyFromValue(domain: Domain): Option[Int] = Some(domain.size)

  trait PartitionLattice extends Lattice[Domain#Partition] with BoundedJoinSemilattice[Domain#Partition] {
    def zero = Domain(0).Partition.fromSortedBlocks(Seq.empty)
    // union
    def join(x: Domain#Partition, y: Domain#Partition): Domain#Partition =
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
    def meet(x: Domain#Partition, y: Domain#Partition): Domain#Partition =
      if (x.size > y.size) meet(y, x) else {
        val refinement = PartitionRefinement(x)
        y.blocks.foreach( block => refinement.refine(block.filter(_ < x.size)) )
        refinement.partition(x.domain)
      }
  }

  trait PartitionPartialOrder extends PartialOrder[Domain#Partition] {
    override def eqv(x: Domain#Partition, y: Domain#Partition): Boolean =
      (x.size == y.size) && (x.blocks.sameElements(y.blocks))

    override def lteqv(x: Domain#Partition, y: Domain#Partition): Boolean =
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

    def partialCompare(x: Domain#Partition, y: Domain#Partition): Double = {
      val xLTEy = lteqv(x, y)
      val yLTEx = lteqv(y, x)
      if (xLTEy) {
        if (yLTEx) 0.0 else -1.0
      } else {
        if (yLTEx) 1.0 else Double.NaN
      }
    }
  }

  implicit object PartitionAlgebra extends PartitionPartialOrder with PartitionLattice

  val empty = Domain(0)

  trait PartitionT {
    def fromSeq(seq: Seq[Any]): Domain#Partition
    def empty: Domain#Partition
    def apply(sets: Set[Int]*): Domain#Partition
  }

  val Partition = new PartitionT {
    def apply(sets: Set[Int]*): Domain#Partition = {
      val domain = Domain((0 /: sets) { case (mx, set) => mx.max(set.max + 1) } )
      domain.Partition(sets:_*)
    }
    def fromSeq(seq: Seq[Any]): Domain#Partition = Domain(seq.size).Partition.fromSeq(seq)
    def empty = new Domain.empty.Partition(new Array[Int](0), new Array[Int](0), new Array[Int](0))
  }

  trait PartitionMapT {
    def apply[V : ClassTag](blocks: (Set[Int], V)*): Domain#PartitionMap[V]
    def empty[V : ClassTag]: Domain#PartitionMap[V]
    def fill[V: ClassTag](partition: Domain#Partition)(elem: => V): Domain#PartitionMap[V]
    def tabulate[V: ClassTag](partition: Domain#Partition)(f: Set[Int] => V): Domain#PartitionMap[V]
  }

  val PartitionMap = new PartitionMapT {
    def apply[V : ClassTag](blocks: (Set[Int], V)*) = {
      val domain = Domain((0 /: blocks) { case (acc, block) => acc.max(block._1.max + 1) })
      domain.PartitionMap(blocks:_*)
    }
    def empty[V : ClassTag]: Domain#PartitionMap[V] = new Domain.empty.PartitionMap(Domain.Partition.empty, new Array[V](0))
    def fill[V : ClassTag](partition: Domain#Partition)(elem: => V): Domain#PartitionMap[V] = {
      val partition.domain.Typed(typedPartition) = partition
      new partition.domain.PartitionMap(typedPartition, Array.fill(partition.nBlocks)(elem))
    }
    def tabulate[V : ClassTag](partition: Domain#Partition)(f: Set[Int] => V): Domain#PartitionMap[V] = {
      val partition.domain.Typed(typedPartition) = partition
      new partition.domain.PartitionMap(typedPartition, Array.tabulate(partition.nBlocks)(i => f(partition.blocks(i))))
    }
  }

  trait PartitionMapPartialOrder[V] extends Any with PartialOrder[Domain#PartitionMap[V]] {
    implicit def partialOrder: PartialOrder[V]
    override def lteqv(x: Domain#PartitionMap[V], y: Domain#PartitionMap[V]): Boolean = // x <= y ?
      if (x.partition <= y.partition) {
        x.partition.blocks.forall { xBlock =>
          val m = xBlock.min
          x(m) <= y(m)
        }
      } else false
    override def gteqv(x: Domain#PartitionMap[V], y: Domain#PartitionMap[V]): Boolean = lteqv(y, x)
    def partialCompare(x: Domain#PartitionMap[V], y: Domain#PartitionMap[V]): Double =
      if (lteqv(x, y)) {
        if (gteqv(x, y)) 0.0 else -1.0
      } else {
        if (gteqv(x, y)) 1.0 else Double.NaN // not x <= y but x >= y
      }
  }

  trait PartitionMapJoinSemilattice[V] extends Any with JoinSemilattice[Domain#PartitionMap[V]] {
    implicit def classTag: ClassTag[V]
    implicit def lattice: JoinSemilattice[V]
    def join(x: Domain#PartitionMap[V], y: Domain#PartitionMap[V]): Domain#PartitionMap[V] =
      if (y.partition.size > x.partition.size) join(y, x) else {
        val newPartition = x.partition join y.partition
        Domain.PartitionMap.tabulate[V](newPartition) { block =>
          val xBlocks = x.partition.blocksFor(block)
          val afterX = (x(xBlocks.head.min) /: xBlocks.tail) {
            case (v, b) => v join x(b.min)
          }

          (afterX /: y.partition.blocksFor(block)) {
            case (v, b) => v join y(b.min)
          }
        }
      }
  }

  trait PartitionMapMeetSemilattice[V] extends Any with MeetSemilattice[Domain#PartitionMap[V]] {
    implicit def classTag: ClassTag[V]
    implicit def lattice: MeetSemilattice[V]

    def meet(x: Domain#PartitionMap[V], y: Domain#PartitionMap[V]): Domain#PartitionMap[V] = {
      val newPartition = x.partition meet y.partition
      Domain.PartitionMap.tabulate[V](newPartition) { block =>
        val xBlocks = x.partition.blocksFor(block)
        val afterX = (x(xBlocks.head.min) /: xBlocks.tail) {
          case (v, b) => v meet x(b.min)
        }

        (afterX /: y.partition.blocksFor(block)) {
          case (v, b) => v meet y(b.min)
        }
      }
    }
  }

  trait PartitionMapLattice[V] extends Any with Lattice[Domain#PartitionMap[V]]
      with PartitionMapJoinSemilattice[V] with PartitionMapMeetSemilattice[V] {
    implicit def lattice: Lattice[V]
  }

  trait PartitionMapBoundedJoinSemilattice[V] extends Any with BoundedJoinSemilattice[Domain#PartitionMap[V]]
      with PartitionMapJoinSemilattice[V] {
    def zero: Domain#PartitionMap[V] = Domain.PartitionMap.empty[V]
  }

  trait PartitionMapBoundedJoinSemilatticeNonEmpty[V] extends Any with BoundedJoinSemilattice[Domain#PartitionMap[V]]
      with PartitionMapJoinSemilattice[V] {
    implicit def lattice: BoundedJoinSemilattice[V]

    def zero: Domain#PartitionMap[V] = Domain.PartitionMap(Set(0) -> lattice.zero)
  }

  trait PartitionMapBoundedBelowLattice[V] extends Any
      with PartitionMapLattice[V]
      with PartitionMapBoundedJoinSemilattice[V] {
    implicit def lattice: Lattice[V]
  }

  trait PartitionMapBoundedBelowLatticeNonEmpty[V] extends Any
      with PartitionMapLattice[V]
      with PartitionMapBoundedJoinSemilatticeNonEmpty[V] {
    implicit def lattice: Lattice[V] with BoundedJoinSemilattice[V]
  }

  implicit def PartitionMapPartialOrder[V: PartialOrder]: PartialOrder[Domain#PartitionMap[V]] =
    new PartitionMapPartialOrder[V] {
      def partialOrder = implicitly[PartialOrder[V]]
    }

  implicit def PartitionMapJoinSemilattice[V : ClassTag : JoinSemilattice]: JoinSemilattice[Domain#PartitionMap[V]] =
    new PartitionMapJoinSemilattice[V] {
      def classTag = implicitly[ClassTag[V]]
      def lattice = implicitly[JoinSemilattice[V]]
    }

  implicit def PartitionMapMeetSemilattice[V : ClassTag : MeetSemilattice]: MeetSemilattice[Domain#PartitionMap[V]] =
    new PartitionMapMeetSemilattice[V] {
      def classTag = implicitly[ClassTag[V]]
      def lattice = implicitly[MeetSemilattice[V]]
    }

  implicit def PartitionMapLattice[V : ClassTag: Lattice]: Lattice[Domain#PartitionMap[V]] =
    new PartitionMapBoundedBelowLattice[V] {
      def classTag = implicitly[ClassTag[V]]
      def lattice = implicitly[Lattice[V]]
    }

  def PartitionMapBoundedJoinSemilattice[V : ClassTag : JoinSemilattice]: BoundedJoinSemilattice[Domain#PartitionMap[V]] =
    new PartitionMapBoundedJoinSemilattice[V] {
      def classTag = implicitly[ClassTag[V]]
      def lattice = implicitly[JoinSemilattice[V]]
    }

  def PartitionMapBoundedJoinSemilatticeNonEmpty[V : ClassTag : BoundedJoinSemilattice]: BoundedJoinSemilattice[Domain#PartitionMap[V]] =
    new PartitionMapBoundedJoinSemilatticeNonEmpty[V] {
      def classTag = implicitly[ClassTag[V]]
      def lattice = implicitly[BoundedJoinSemilattice[V]]
    }

  def PartitionMapBoundedBelowLattice[V : ClassTag: Lattice]: Lattice[Domain#PartitionMap[V]] with BoundedJoinSemilattice[Domain#PartitionMap[V]] =
    new PartitionMapBoundedBelowLattice[V] {
      def classTag = implicitly[ClassTag[V]]
      def lattice = implicitly[Lattice[V]]
    }

  def PartitionMapBoundedBelowLatticeNonEmpty[V : ClassTag](implicit ev: Lattice[V] with BoundedJoinSemilattice[V]): Lattice[Domain#PartitionMap[V]] with BoundedJoinSemilattice[Domain#PartitionMap[V]] =
    new PartitionMapBoundedBelowLatticeNonEmpty[V] {
      def classTag = implicitly[ClassTag[V]]
      def lattice = ev
    }
}
