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

final class PartitionMap[V: ClassTag](val partition: Domain#Partition, protected val values: Array[V]) {
  override def toString = partition.blocks.map( block => block.toString + " -> " + apply(block).toString).mkString("PartitionMap(", ", ", ")")
  def getOrElse(i: Int, defaultValue: => V) =
    if (i < partition.size) apply(i) else defaultValue
  def apply(i: Int): V =
    if (i < 0 || i >= partition.size) throw new IndexOutOfBoundsException(i.toString) else
      values(partition.blockIndex(i))
  def apply(block: Set[Int]): V =
    if (block.isEmpty) throw new NoSuchElementException(block.toString) else {
      val m = block.min
      val bi = partition.blockIndex(m)
      if (partition.blocks(bi) != block)
        throw new NoSuchElementException(block.toString)
      else
        values(bi)
    }
  def isDefinedAt(i: Int): Boolean = (i >= 0 && i < partition.size)
  def isDefinedAt(block: Set[Int]): Boolean = block.nonEmpty && partition.blocks(partition.blockIndex(block.min)) == block
  def forPartitionSize(newSize: Int, defaultValue: => V, removedSatisfy: V => Boolean): RefOption[PartitionMap[V]] =
    if (newSize == partition.size)
      RefSome(this)
    else if (newSize > partition.size) {
      val newPartition = partition.inDomain(Domain(newSize)).get
      val newValues = values ++ Array.fill(newPartition.numBlocks - partition.numBlocks)(defaultValue)
      RefSome(new PartitionMap(newPartition, newValues))
    } else // newSize < partition.size
      partition.inDomain(Domain(newSize)) match {
        case RefOption(newPartition) =>
          if ((newSize until partition.size).forall( i => removedSatisfy(apply(i)) ))
            RefSome(new PartitionMap(newPartition, values.take(values.length - (partition.size - newSize))))
          else
            RefNone
        case _ => RefNone
      }
}
final class PartitionMapLattice[V: ClassTag](implicit val scalar: BoundedBelowLattice[V]) extends BoundedBelowLattice[PartitionMap[V]] {
  def zero = new PartitionMap(Domain(1).Partition.Algebra.one, Array(scalar.zero))
  def join(x: PartitionMap[V], y: PartitionMap[V]): PartitionMap[V] = {
    val newSize = x.partition.size.max(y.partition.size)
    val domain = Domain(newSize)
    import domain.Partition.Algebra
    val newPartition = x.partition.inDomain(domain).get join y.partition.inDomain(domain).get
    PartitionMap.tabulate[V](newPartition) { block =>
      val afterX = (scalar.zero /: x.partition.blocksFor(block)) {
        case (v, b) => v join x(b.min)
      }

      (afterX /: y.partition.blocksFor(block)) {
        case (v, b) => v join y(b.min)
      }
    }
  }
  def meet(x: PartitionMap[V], y: PartitionMap[V]): PartitionMap[V] = {
    val tempSize = x.partition.size.max(y.partition.size)
    val tempDomain = Domain(tempSize)
    import tempDomain.Partition.Algebra
    val tempPartition = x.partition.inDomain(tempDomain).get meet y.partition.inDomain(tempDomain).get
    val newSize = x.partition.size.min(y.partition.size)
    val newDomain = Domain(newSize)
    val newPartition = tempPartition.inDomain(newDomain).get
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
  override def lteqv(x: PartitionMap[V], y: PartitionMap[V]): Boolean = // x <= y ?
    if (y.partition.size < x.partition.size) false else {
      val xSized = x.forPartitionSize(y.partition.size, scalar.zero, scalar.isZero(_)).get
      val domain = Domain(y.partition.size)
      if (!(xSized.partition.inDomain(domain).get <= y.partition.inDomain(domain).get))
        false
      else
        xSized.partition.blocks.forall { xBlock =>
          val m = xBlock.min
          x.getOrElse(m, scalar.zero) <= y(m)
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

object PartitionMap {
  implicit def PartitionMapLattice[V: ClassTag: BoundedBelowLattice]: BoundedBelowLattice[PartitionMap[V]] = new PartitionMapLattice[V]
  def fill[V: ClassTag](partition: Domain#Partition)(elem: => V): PartitionMap[V] =
    new PartitionMap(partition, Array.fill(partition.numBlocks)(elem))
  def tabulate[V: ClassTag](partition: Domain#Partition)(f: Set[Int] => V): PartitionMap[V] =
    new PartitionMap(partition, Array.tabulate(partition.numBlocks)(i => f(partition.blocks(i))))
}

final class Domain private (val size: Int) {
  override def toString = s"Domain($size)"
  /** Represents an union of disjoint subsets. The subsets are internally
    * stored in an array, sorted by their minimal element.
    */
  final class Partition(protected[alasc] val array: Array[immutable.BitSet]) {
    override def toString = blocks.map(_.mkString("[", " ", "]")).mkString
    @inline def domain: Domain = Domain.this
    @inline def size: Int = Domain.this.size
    /** Returns the partition in a domain of possibly a different size. The resized part, to be
      * added or removed, is composed of single points. Returns RefNone if the resizing is not possible,
      * e.g. because the removed last points are not single in a partition.
      */
    def inDomain(newDomain: Domain): RefOption[newDomain.Partition] =
      if (newDomain.size == size)
        RefSome(Partition.this.asInstanceOf[newDomain.Partition])
      else if (newDomain.size > size) {
        val numEndPoints = newDomain.size - size
        val newArray = new Array[immutable.BitSet](numBlocks + numEndPoints)
        Array.copy(array, 0, newArray, 0, array.length)
        val start = array.length
        var i = start
        while (i < newArray.length) {
          newArray(i) = immutable.BitSet(size + (i - start))
          i += 1
        }
        RefSome(new newDomain.Partition(newArray))
      } else { // newDomain.size < size
        val numEndPoints = size - newDomain.size
        val start = array.length - numEndPoints
        var i = start
        if (i < 0) return RefNone
        while (i < array.length) {
          val point = newDomain.size + (i - start)
          if (array(i).size != 1 || array(i).min != point)
            return RefNone
          i += 1
        }
        val newArray = new Array[immutable.BitSet](numBlocks - numEndPoints)
        Array.copy(array, 0, newArray, 0, numBlocks - numEndPoints)
        RefSome(new newDomain.Partition(newArray))
      }
    /** Returns the number of blocks. */
    def numBlocks = array.length
    /** Returns the blocks in the partition, ordered by their minimal element. */
    def blocks: Seq[Set[Int]] = array
    /** Returns the blocks that intersect the set `points`. */
    def blocksFor(points: Set[Int]): Seq[Set[Int]] = {
      val buf = mutable.ArrayBuffer.empty[Set[Int]]
      var remaining = mutable.BitSet.empty ++= points
      while (remaining.nonEmpty && remaining.min < size) {
        val m = remaining.min
        val b = blockFor(m)
        buf += b
        remaining --= b
      }
      buf.result
    }
    /** Returns the block in which `k` is contained. Must have `0 <= k < size`. */
    def blockFor(k: Int): Set[Int] = array(blockIndex(k))
    lazy val blockIndex: Array[Int] = {
      val res = new Array[Int](size)
      var i = 0
      while (i < array.length) {
        array(i).foreach { k => res(k) = i }
        i += 1
      }
      res
    }
    /** Returns the minimal representative of the block in which `k` is contained.
      * Must have `0 <= k < size`.
      */
    def representative(k: Int): Int = blockFor(k).min
    /** Returns the sequence of blocks, the block size increasing. */
    def sizeIncreasing: Seq[Set[Int]] = array.sortBy(b => (b.size, b.min))
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
      fromArray(blocks.toArray)
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
      fromArray(blocks.map(_.toImmutable).toArray)
    }
    def unapply(gen: Domain#Partition): RefOption[Partition] =
      if (gen.domain eq Domain.this) RefSome(gen.asInstanceOf[Partition]) else RefNone
    def fromArray(array: Array[immutable.BitSet]) = new Partition(array)
    implicit object Algebra extends BoundedLattice[Partition] {
      def zero = Partition.fromArray(Array.tabulate(size)( i => immutable.BitSet(i) ))
      def one = Partition.fromArray(Array(immutable.BitSet.empty ++ (0 until size)))
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
            if (x.array.sameElements(y.array)) 0.0 else Double.NaN
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
