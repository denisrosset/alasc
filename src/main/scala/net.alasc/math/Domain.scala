package net.alasc.math

import scala.annotation.tailrec

import scala.collection.BitSet
import scala.collection.mutable
import scala.collection.immutable

import spire.algebra.PartialOrder

import net.alasc.algebra.BoundedLattice
import net.alasc.util._
import partition._

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
  implicit val lattice: BoundedLattice[Partition] = new BoundedLattice[Partition] {
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

object Domain extends UniquenessCache[Int, Domain] {
  protected def valueFromKey(size: Int): Domain = new Domain(size)
  protected def keyFromValue(domain: Domain): Option[Int] = Some(domain.size)
}

object Partition {
  def fromSeq(seq: Seq[Any]): Domain#Partition = Domain(seq.size).Partition.fromSeq(seq)
}
