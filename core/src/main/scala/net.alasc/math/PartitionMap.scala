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

final class PartitionMapBuilder[D <: Domain with Singleton](val domain: D) extends AnyVal {

  def apply[V:ClassTag](blocks: (Set[Int], V)*): PartitionMap[D, V] = {
    val map = blocks.toMap
    val partition = (domain: D).Partition(blocks.map(_._1):_*)
    tabulate(partition)(map(_))
  }

    def fill[V:ClassTag](partition: Partition[D])(elem: => V): PartitionMap[D, V] =
      new PartitionMap[D, V](domain, partition, Array.fill(partition.nBlocks)(elem))

  def tabulate[V:ClassTag](partition: Partition[D])(f: Set[Int] => V): PartitionMap[D, V] =
    new PartitionMap[D, V](domain, partition, Array.tabulate(partition.nBlocks)(i => f(partition.blocks(i))))

}

/** Describes a partition of the domain with a value `V` associated to each block. */
final class PartitionMap[D <: Domain with Singleton, V:ClassTag](val domain: D, val partition: Partition[D], protected val values: Array[V]) extends InDomain[D] {

  require(partition.domain eq domain)

  /** Returns the size of the underlying domain. */
  def size = domain.size

  def blocks: Seq[(Set[Int], V)] = partition.blocks.map(block => (block -> PartitionMap.this(block)))
  override def equals(other: Any) = other match {
    case that: PartitionMap[_, _] => (this.partition == that.partition) && (this.partition.blocks.forall(block => this(block) == that(block)))
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
  def forDomain(newDomain: Domain, defaultValue: => V, removedSatisfy: V => Boolean): Opt[PartitionMap[newDomain.type, V]] =
    if (newDomain.size == size)
      Opt(this.asInstanceOf[PartitionMap[newDomain.type, V]])
    else if (newDomain.size > size) {
      val newPartition = partition.inDomain(newDomain).get
      val newValues = values ++ Array.fill(newPartition.nBlocks - partition.nBlocks)(defaultValue)
      Opt(new PartitionMap[newDomain.type, V](newDomain, newPartition, newValues))
    } else // newSize < partition.size
      partition.inDomain(newDomain) match {
        case Opt(newPartition) =>
          if ((newDomain.size until size).forall( i => removedSatisfy(apply(i)) ))
            Opt(new PartitionMap[newDomain.type, V](newDomain, newPartition, values.take(values.length - (size - newDomain.size))))
          else
            Opt.empty[PartitionMap[newDomain.type, V]]
        case _ => Opt.empty[PartitionMap[newDomain.type, V]]
      }

}

trait PartitionMapPartialOrder[D <: Domain with Singleton, V] extends PartialOrder[PartitionMap[D, V]] with InDomain[D] {

  implicit def partialOrder: PartialOrder[V]

  override def lteqv(x: PartitionMap[D, V], y: PartitionMap[D, V]): Boolean = // x <= y ?
    if (!(x.partition <= y.partition))
      false
    else {
      x.partition.blocks.forall { xBlock =>
        val m = xBlock.min
        x(m) <= y(m)
      }
    }

  override def gteqv(x: PartitionMap[D, V], y: PartitionMap[D, V]): Boolean = lteqv(y, x)

  def partialCompare(x: PartitionMap[D, V], y: PartitionMap[D, V]): Double =
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

trait PartitionMapJoinSemilattice[D <: Domain with Singleton, V] extends JoinSemilattice[PartitionMap[D, V]] with InDomain[D] {

  implicit def classTag: ClassTag[V]

  implicit def lattice: JoinSemilattice[V]

  def join(x: PartitionMap[D, V], y: PartitionMap[D, V]): PartitionMap[D, V] = {
    val newPartition = x.partition join y.partition
    (domain: D).PartitionMap.tabulate[V](newPartition) { block =>
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

trait PartitionMapMeetSemilattice[D <: Domain with Singleton, V] extends MeetSemilattice[PartitionMap[D, V]] with InDomain[D] {

  implicit def classTag: ClassTag[V]

  implicit def lattice: MeetSemilattice[V]

  def meet(x: PartitionMap[D, V], y: PartitionMap[D, V]): PartitionMap[D, V] = {
    val newPartition = x.partition meet y.partition
    (domain: D).PartitionMap.tabulate[V](newPartition) { block =>
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
trait PartitionMapLattice[D <: Domain with Singleton, V]
    extends Lattice[PartitionMap[D, V]]
    with PartitionMapJoinSemilattice[D, V]
    with PartitionMapMeetSemilattice[D, V] {

  implicit def lattice: Lattice[V]

}

trait PartitionMapBoundedJoinSemilattice[D <: Domain with Singleton, V]
    extends BoundedJoinSemilattice[PartitionMap[D, V]]
    with PartitionMapJoinSemilattice[D, V] {

  implicit def lattice: BoundedJoinSemilattice[V]

  def zero = (domain: D).PartitionMap.fill(implicitly[BoundedLattice[Partition[D]]].zero)(lattice.zero)

}

trait PartitionMapBoundedMeetSemilattice[D <: Domain with Singleton, V]
    extends BoundedMeetSemilattice[PartitionMap[D, V]]
    with PartitionMapMeetSemilattice[D, V] {

  implicit def lattice: BoundedMeetSemilattice[V]

  def one = (domain: D).PartitionMap.fill(implicitly[BoundedLattice[Partition[D]]].one)(lattice.one)

}

trait PartitionMapBoundedLattice[D <: Domain with Singleton, V]
    extends BoundedLattice[PartitionMap[D, V]]
    with PartitionMapBoundedJoinSemilattice[D, V]
    with PartitionMapBoundedMeetSemilattice[D, V] {

  implicit def lattice: BoundedLattice[V]

}

object PartitionMap {

  implicit def partialOrder[D <: Domain with Singleton, V:PartialOrder](implicit witness: shapeless.Witness.Aux[D]): PartialOrder[PartitionMap[D, V]] = new PartitionMapPartialOrder[D, V] {

    val domain = witness.value

    def partialOrder = implicitly[PartialOrder[V]]

  }

  /*
    /** Induced lattices for partition maps, when the values are themselves member of a lattice. */
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
*/
}
