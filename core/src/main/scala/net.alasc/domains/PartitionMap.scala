package net.alasc.domains

import scala.reflect.ClassTag

import spire.algebra.PartialOrder
import spire.algebra.lattice._
import spire.syntax.partialOrder._
import spire.syntax.lattice._
import spire.util.Opt

/** Describes a partition of the domain with a value `V` associated to each block. */
trait PartitionMap[V] extends InDomain {

  type InAnother[D0 <: Domain with Singleton] = PartitionMap.In[D0, V]

  implicit def classTag: ClassTag[V]

  val partition: Partition.In[D]

  protected val values: Array[V]

  /** Returns the size of the underlying domain. */
  def size = domain.size

  def blocks: Seq[(Set[Int], V)] = partition.blocks.map(block => (block -> PartitionMap.this(block)))
  override def equals(other: Any) = other match {
    case that: PartitionMap[_] => (this.partition == that.partition) && (this.partition.blocks.forall(block => this(block) == that(block)))
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

  /** Returns a resized partition map, using `defaultValue` when expanding, and testing whether
    * elements can be removed using `removedSatisfy`. 
    * Returns `Opt.empty` if the resizing is not possible. */
  def forDomain(newDomain: Domain, defaultValue: => V, removedSatisfy: V => Boolean): Opt[PartitionMap.In[newDomain.type, V]] =
    if (newDomain.size == size)
      Opt(this.asInstanceOf[PartitionMap.In[newDomain.type, V]])
    else if (newDomain.size > size) {
      val newPartition = partition.inDomain(newDomain).get
      val newValues = values ++ Array.fill(newPartition.nBlocks - partition.nBlocks)(defaultValue)
      Opt(PartitionMap.build[V](newDomain)(newPartition, newValues))
    } else // newSize < partition.size
      partition.inDomain(newDomain) match {
        case Opt(newPartition) =>
          if ((newDomain.size until size).forall( i => removedSatisfy(apply(i)) ))
            Opt(PartitionMap.build[V](newDomain)(newPartition, values.take(values.length - (size - newDomain.size))))
          else
            Opt.empty[PartitionMap.In[newDomain.type, V]]
        case _ => Opt.empty[PartitionMap.In[newDomain.type, V]]
      }

}

abstract class PartitionMapLowerPriority {

  implicit def partialOrder[V:PartialOrder]: PartialOrder[PartitionMap[V]] = new PartitionMapPartialOrder[V]

  implicit def joinSemilattice[V:ClassTag:JoinSemilattice]: JoinSemilattice[PartitionMap[V]] =
    new PartitionMapJoinSemilattice[V] {
      def classTag = implicitly[ClassTag[V]]
      def lattice = implicitly[JoinSemilattice[V]]
    }

  implicit def meetSemilattice[V:ClassTag:MeetSemilattice]: MeetSemilattice[PartitionMap[V]] =
    new PartitionMapMeetSemilattice[V] {
      def classTag = implicitly[ClassTag[V]]
      def lattice = implicitly[MeetSemilattice[V]]
    }

  implicit def lattice[V:ClassTag:Lattice]: Lattice[PartitionMap[V]] =
    new PartitionMapBoundedBelowLattice[V] {
      def classTag = implicitly[ClassTag[V]]
      def lattice = implicitly[Lattice[V]]
    }

  def boundedJoinSemilattice[V:ClassTag:JoinSemilattice]: BoundedJoinSemilattice[PartitionMap[V]] =
    new PartitionMapBoundedJoinSemilattice[V] {
      def classTag = implicitly[ClassTag[V]]
      def lattice = implicitly[JoinSemilattice[V]]
    }

  def boundedJoinSemilatticeNonEmpty[V:ClassTag:BoundedJoinSemilattice]: BoundedJoinSemilattice[PartitionMap[V]] =
    new PartitionMapBoundedJoinSemilatticeNonEmpty[V] {
      def classTag = implicitly[ClassTag[V]]
      def lattice = implicitly[BoundedJoinSemilattice[V]]
    }

  def boundedBelowLattice[V:ClassTag:Lattice]: Lattice[PartitionMap[V]] with BoundedJoinSemilattice[PartitionMap[V]] =
    new PartitionMapBoundedBelowLattice[V] {
      def classTag = implicitly[ClassTag[V]]
      def lattice = implicitly[Lattice[V]]
    }

  def boundedBelowLatticeNonEmpty[V:ClassTag](implicit ev: Lattice[V] with BoundedJoinSemilattice[V]): Lattice[PartitionMap[V]] with BoundedJoinSemilattice[PartitionMap[V]] =
    new PartitionMapBoundedBelowLatticeNonEmpty[V] {
      def classTag = implicitly[ClassTag[V]]
      def lattice = ev
    }

}

object PartitionMap extends PartitionMapLowerPriority {

  type In[D0 <: Domain with Singleton, V] = PartitionMap[V] { type D = D0 }

  protected def build[V:ClassTag](domain0: Domain)(partition0: Partition.In[domain0.type], values0: Array[V]): PartitionMap.In[domain0.type, V] = new PartitionMap[V] {
    type D = domain0.type
    val domain: D = domain0
    val classTag: ClassTag[V] = implicitly[ClassTag[V]]
    val partition = partition0
    val values = values0
  }

  def empty[V:ClassTag]: PartitionMap[V] = apply[V]()

  def apply[V:ClassTag](blocks: (Set[Int], V)*): PartitionMap[V] = {
    val domain = Domain((0 /: blocks) { case (acc, block) => acc.max(block._1.max + 1) })
    apply[V](domain)(blocks: _*)
  }

  def apply[V:ClassTag](domain: Domain)(blocks: (Set[Int], V)*): PartitionMap.In[domain.type, V] = {
    val map = blocks.toMap
    val partition = Partition(domain)(blocks.map(_._1):_*)
    tabulate(partition)(map(_))
  }

  def tabulate[V:ClassTag](partition: Partition)(f: Set[Int] => V): PartitionMap.In[partition.D, V] =
    build(partition.domain: partition.D)(partition, Array.tabulate(partition.nBlocks)(i => f(partition.blocks(i))))

  def fill[V:ClassTag](partition: Partition)(elem: => V): PartitionMap.In[partition.D, V] =
    tabulate(partition)(block => elem)

  implicit def partialOrderIn[D <: Domain with Singleton, V:PartialOrder](implicit witness: shapeless.Witness.Aux[D]): PartialOrder[PartitionMap.In[D, V]] = {
    val domain0: D = witness.value
    new PartitionMapPartialOrderIn[D, V](domain0)
  }

  // Induced lattices for partition maps, when the values are themselves member of a lattice.

  implicit def joinSemilatticeIn[D <: Domain with Singleton, V:ClassTag:JoinSemilattice](implicit witness: shapeless.Witness.Aux[D]): JoinSemilattice[PartitionMap.In[D, V]] = {
    val domain0: D = witness.value
    new PartitionMapJoinSemilatticeIn[D, V] {
      val domain: D = domain0
      def classTag = implicitly[ClassTag[V]]
      def lattice = implicitly[JoinSemilattice[V]]
    }
  }

  implicit def meetSemilatticeIn[D <: Domain with Singleton, V:ClassTag:MeetSemilattice](implicit witness: shapeless.Witness.Aux[D]): MeetSemilattice[PartitionMap.In[D, V]] = {
    val domain0: D = witness.value
    new PartitionMapMeetSemilatticeIn[D, V] {
      val domain: D = domain0
      def classTag = implicitly[ClassTag[V]]
      def lattice = implicitly[MeetSemilattice[V]]
    }
  }

  implicit def latticeIn[D <: Domain with Singleton, V:ClassTag:Lattice](implicit witness: shapeless.Witness.Aux[D]): Lattice[PartitionMap.In[D, V]] = {
    val domain0: D = witness.value
    new PartitionMapLatticeIn[D, V] {
      val domain: D = domain0
      def classTag = implicitly[ClassTag[V]]
      def lattice = implicitly[Lattice[V]]
    }
  }

  implicit def boundedJoinSemilatticeIn[D <: Domain with Singleton, V:ClassTag:BoundedJoinSemilattice](implicit witness: shapeless.Witness.Aux[D]): BoundedJoinSemilattice[PartitionMap.In[D, V]] = {
    val domain0: D = witness.value
    new PartitionMapBoundedJoinSemilatticeIn[D, V] {
      val domain: D = domain0
      def classTag = implicitly[ClassTag[V]]
      def lattice = implicitly[BoundedJoinSemilattice[V]]
    }
  }

  implicit def boundedMeetSemilatticeIn[D <: Domain with Singleton, V:ClassTag:BoundedMeetSemilattice](implicit witness: shapeless.Witness.Aux[D]): BoundedMeetSemilattice[PartitionMap.In[D, V]] = {
    val domain0: D = witness.value
    new PartitionMapBoundedMeetSemilatticeIn[D, V] {
      val domain: D = domain0
      def classTag = implicitly[ClassTag[V]]
      def lattice = implicitly[BoundedMeetSemilattice[V]]
    }
  }

  implicit def boundedLatticeIn[D <: Domain with Singleton, V:ClassTag:BoundedLattice](implicit witness: shapeless.Witness.Aux[D]): BoundedLattice[PartitionMap.In[D, V]] = {
    val domain0: D = witness.value
    new PartitionMapBoundedLatticeIn[D, V] {
      val domain: D = domain0
      def classTag = implicitly[ClassTag[V]]
      def lattice = implicitly[BoundedLattice[V]]
    }
  }

}

final class PartitionMapPartialOrder[V](implicit val partialOrder: PartialOrder[V])
    extends PartialOrder[PartitionMap[V]] {

  override def lteqv(x: PartitionMap[V], y: PartitionMap[V]): Boolean = // x <= y ?
    if ((x.partition: Partition) <= y.partition) {
      x.partition.blocks.forall { xBlock =>
        val m = xBlock.min
        x(m) <= y(m)
      }
    } else false

  override def gteqv(x: PartitionMap[V], y: PartitionMap[V]): Boolean = lteqv(y, x)

  def partialCompare(x: PartitionMap[V], y: PartitionMap[V]): Double =
    if (lteqv(x, y)) {
      if (gteqv(x, y)) 0.0 else -1.0
    } else {
      if (gteqv(x, y)) 1.0 else Double.NaN // not x <= y but x >= y
    }

}

final class PartitionMapPartialOrderIn[D <: Domain with Singleton, V](val domain: D)(implicit val partialOrder: PartialOrder[V])
    extends PartialOrder[PartitionMap.In[D, V]]
    with InDomain.Of[D] {

  override def lteqv(x: PartitionMap.In[D, V], y: PartitionMap.In[D, V]): Boolean = // x <= y ?
    if (!(x.partition <= y.partition))
      false
    else {
      x.partition.blocks.forall { xBlock =>
        val m = xBlock.min
        x(m) <= y(m)
      }
    }

  override def gteqv(x: PartitionMap.In[D, V], y: PartitionMap.In[D, V]): Boolean = lteqv(y, x)

  def partialCompare(x: PartitionMap.In[D, V], y: PartitionMap.In[D, V]): Double =
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

trait PartitionMapJoinSemilattice[V]
    extends JoinSemilattice[PartitionMap[V]] {

  implicit def classTag: ClassTag[V]

  implicit def lattice: JoinSemilattice[V]

  def join(x: PartitionMap[V], y: PartitionMap[V]): PartitionMap[V] =
    if (y.partition.size > x.partition.size) join(y, x) else {
      val newPartition = (x.partition: Partition) join y.partition
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

trait PartitionMapMeetSemilattice[V] extends MeetSemilattice[PartitionMap[V]] {

  implicit def classTag: ClassTag[V]

  implicit def lattice: MeetSemilattice[V]

  def meet(x: PartitionMap[V], y: PartitionMap[V]): PartitionMap[V] = {
    val newPartition = (x.partition: Partition) meet y.partition
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

trait PartitionMapLattice[V]
    extends Lattice[PartitionMap[V]]
    with PartitionMapJoinSemilattice[V]
    with PartitionMapMeetSemilattice[V] {

  implicit def lattice: Lattice[V]

}

trait PartitionMapBoundedJoinSemilattice[V]
    extends BoundedJoinSemilattice[PartitionMap[V]]
    with PartitionMapJoinSemilattice[V] {

  def zero: PartitionMap[V] = PartitionMap.empty[V]

}

trait PartitionMapBoundedJoinSemilatticeNonEmpty[V]
    extends BoundedJoinSemilattice[PartitionMap[V]]
    with PartitionMapJoinSemilattice[V] {

  implicit def lattice: BoundedJoinSemilattice[V]

  def zero: PartitionMap[V] = PartitionMap(Set(0) -> lattice.zero)

}

trait PartitionMapBoundedBelowLattice[V]
    extends PartitionMapLattice[V]
    with PartitionMapBoundedJoinSemilattice[V] {

  implicit def lattice: Lattice[V]

}

trait PartitionMapBoundedBelowLatticeNonEmpty[V]
    extends PartitionMapLattice[V]
    with PartitionMapBoundedJoinSemilatticeNonEmpty[V] {

  implicit def lattice: Lattice[V] with BoundedJoinSemilattice[V]

}

trait PartitionMapJoinSemilatticeIn[D <: Domain with Singleton, V]
    extends JoinSemilattice[PartitionMap.In[D, V]]
    with InDomain.Of[D] {

  implicit def classTag: ClassTag[V]

  implicit def lattice: JoinSemilattice[V]

  def join(x: PartitionMap.In[D, V], y: PartitionMap.In[D, V]): PartitionMap.In[D, V] = {
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

trait PartitionMapMeetSemilatticeIn[D <: Domain with Singleton, V]
    extends MeetSemilattice[PartitionMap.In[D, V]]
    with InDomain.Of[D] {

  implicit def classTag: ClassTag[V]

  implicit def lattice: MeetSemilattice[V]

  def meet(x: PartitionMap.In[D, V], y: PartitionMap.In[D, V]): PartitionMap.In[D, V] = {
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

trait PartitionMapLatticeIn[D <: Domain with Singleton, V]
    extends Lattice[PartitionMap.In[D, V]]
    with PartitionMapJoinSemilatticeIn[D, V]
    with PartitionMapMeetSemilatticeIn[D, V] {

  implicit def lattice: Lattice[V]

}

trait PartitionMapBoundedJoinSemilatticeIn[D <: Domain with Singleton, V]
    extends BoundedJoinSemilattice[PartitionMap.In[D, V]]
    with PartitionMapJoinSemilatticeIn[D, V] {

  implicit def lattice: BoundedJoinSemilattice[V]

  def zero = PartitionMap.fill(implicitly[BoundedLattice[Partition.In[D]]].zero)(lattice.zero)

}

trait PartitionMapBoundedMeetSemilatticeIn[D <: Domain with Singleton, V]
    extends BoundedMeetSemilattice[PartitionMap.In[D, V]]
    with PartitionMapMeetSemilatticeIn[D, V] {

  implicit def lattice: BoundedMeetSemilattice[V]

  def one = PartitionMap.fill(implicitly[BoundedLattice[Partition.In[D]]].one)(lattice.one)

}

trait PartitionMapBoundedLatticeIn[D <: Domain with Singleton, V]
    extends BoundedLattice[PartitionMap.In[D, V]]
    with PartitionMapBoundedJoinSemilatticeIn[D, V]
    with PartitionMapBoundedMeetSemilatticeIn[D, V] {

  implicit def lattice: BoundedLattice[V]

}
