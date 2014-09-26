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

/** Describes a partition of the set `0 until size`, with a value `V` associated to each block. */
final class PartitionMap[V: ClassTag](val partition: Domain#Partition, protected val values: Array[V]) {
  /** Returns the size of the underlying domain. */
  def size = partition.size
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

/** Induced lattice for partition maps, when the values are themselves member of a lattice bounded below. */
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

