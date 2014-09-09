package net.alasc.algebra

import scala.annotation.tailrec

import scala.collection.BitSet
import scala.collection.mutable
import scala.collection.immutable

import spire.algebra.PartialOrder

import net.alasc.util._

trait Lattice[A] extends PartialOrder[A] {
  def join(x: A, y: A): A
  def meet(x: A, y: A): A
}

class PartitionRefinement(val blocks: debox.Buffer[mutable.BitSet], val size: Int) {
  def refine(x: Set[Int]): Unit = {
    val xBitSet = immutable.BitSet.empty ++ x
    val oldLength = blocks.length
    var i = 0
    while (i < oldLength) {
      val intersection = blocks(i) & xBitSet
      if (!intersection.isEmpty && !(intersection.size == blocks(i).size)) {
        blocks(i) &~= intersection
        blocks += intersection
      }
      i += 1
    }
  }
  def partition: Domain#Partition = Domain(size).Partition.fromArray(blocks.iterator.map(_.toImmutable).toArray)
}

object PartitionRefinement {
  def apply(size: Int) = new PartitionRefinement(debox.Buffer(mutable.BitSet.empty ++= (0 until size)), size)
  def apply(partition: Domain#Partition) = {
    val blocks = debox.Buffer.empty[mutable.BitSet]
    partition.blocks.foreach { block => blocks += (mutable.BitSet.empty ++= block) }
    new PartitionRefinement(blocks, partition.size)
  }
}

/** Implementation of a disjoint-set data structure, using forests.
  * 
  * See http://en.wikipedia.org/wiki/Disjoint-set_data_structure
  * 
  * We implement path compression, but not union by rank.
  */
class DisjointSetForest(val parent: Array[Int]) {
  def size = parent.length
  /** Finds the representative of the set in which `x` is contained. */
  def find(x: Int): Int =
    if (parent(x) != x) {
      val res = find(parent(x))
      parent(x) = res
      res
    } else x
  /** Combines the trees containing `x` and `y`. The representative of the union is chosen as the 
    * minimal representative of `x` and `y`.
    */
  def union(x: Int, y: Int): Unit = {
    val xRoot = find(x)
    val yRoot = find(y)
    if (xRoot < yRoot)
      parent(yRoot) = xRoot
    else if (xRoot > yRoot)
      parent(xRoot) = yRoot
  }
  def partition: Domain#Partition = {
    (0 until size).foreach(find(_))
    Partition.fromSeq(parent)
  }
}

object DisjointSetForest {
  // MakeSet from http://en.wikipedia.org/wiki/Disjoint-set_data_structure
  def apply(size: Int) = new DisjointSetForest(Array.tabulate(size)(identity))
  def apply(partition: Domain#Partition) = new DisjointSetForest(Array.tabulate(partition.size)(partition.representative(_)))
}

object PartitionLattice extends Lattice[Domain#Partition] {
  // union
  def join(x: Domain#Partition, y: Domain#Partition): Domain#Partition = {
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
    forest.partition
  }

  // refinement
  def meet(x: Domain#Partition, y: Domain#Partition): Domain#Partition = {
    assert(x.domain eq y.domain)
    val refinement = PartitionRefinement(x)
    y.blocks.foreach( refinement.refine(_) )
    refinement.partition
  }

  def isRefinementOf(x: Domain#Partition, y: Domain#Partition): Boolean = {
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

  def partialCompare(x: Domain#Partition, y: Domain#Partition): Double = {
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

final class Domain private (val size: Int) {
  override def toString = s"Domain($size)"
  /** Represents an union of disjoint subsets. The subsets are internally
    * stored in an array, sorted by their minimal element.
    */
  final class Partition(protected[alasc] val array: Array[immutable.BitSet]) {
    override def toString = blocks.map(_.mkString("[", " ", "]")).mkString
    @inline def domain: Domain = Domain.this
    @inline def size: Int = Domain.this.size
    def numBlocks = array.length
    def blocks: Iterable[Set[Int]] = (array: Seq[Set[Int]])
    // TODO: optimize by computing indices
    def blockFor(k: Int): Set[Int] = {
      var i = 0
      while (i < array.length) {
        if (array(i).contains(k))
          return array(i)
        i += 1
      }
      sys.error("Point outside domain")
    }
    def representative(k: Int): Int = blockFor(k).min
    def sizeIncreasing: Seq[Set[Int]] = array.toSeq.sortBy(b => (b.size, b.min))
  }
  object Partition {
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
  }
  implicit def lattice: Lattice[Partition] = PartitionLattice.asInstanceOf[Lattice[Partition]]
}

object Domain extends UniquenessCache[Int, Domain] {
  protected def valueFromKey(size: Int): Domain = new Domain(size)
  protected def keyFromValue(domain: Domain): Option[Int] = Some(domain.size)
}

object Partition {
  def fromSeq(seq: Seq[Any]): Domain#Partition = Domain(seq.size).Partition.fromSeq(seq)
}
