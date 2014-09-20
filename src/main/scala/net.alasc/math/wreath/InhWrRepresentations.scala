package net.alasc.math
package wreath

import scala.language.higherKinds

import scala.collection.generic.CanBuildFrom
import scala.collection.SeqLike
import scala.collection.immutable
import scala.collection.mutable
import scala.reflect.ClassTag

import spire.algebra._
import spire.syntax.eq._
import spire.syntax.partialOrder._
import spire.syntax.group._
import spire.syntax.groupAction._

import net.alasc.algebra._
import net.alasc.std.seq._
import net.alasc.syntax.permutationAction._
import net.alasc.syntax.subgroup._
import net.alasc.syntax.lattice._
import net.alasc.util._

abstract class InhWrRepresentations[A, H] extends Representations[Wr[A, H]] {
  selfReps =>
  implicit val aReps: Representations[A]
  implicit def aAlgebra: FiniteGroup[A]
  implicit def hAlgebra: Permutation[H]
  type AR = aReps.R
  implicit def aRepsRClassTag: ClassTag[aReps.R] = aReps.rClassTag
  implicit object lattice extends BoundedBelowLattice[R] {
    def zero = R(Domain(1).Partition.fromArray(Array(immutable.BitSet(0))), Array(aReps.lattice.zero))
    override def lteqv(x: R, y: R): Boolean = // x <= y ?
      if (y.size < x.size) false else {
        val xSized = x.forSize(y.size).get
        val domain = Domain(x.size)
        implicit def aRepLattice: Lattice[aReps.R] = aReps.lattice
        if (!(xSized.partition.inDomain(domain).get <= y.partition.inDomain(domain).get))
          false
        else {
          var i = 0
          while (i < xSized.partition.numBlocks) {
            val m = xSized.partition.blocks(i).min
            if (!(xSized.repForBlock(i) <= y.repForIndex(m)))
              return false
            i += 1
          }
          true
        }
      }
    override def gteqv(x: R, y: R): Boolean = lteqv(y, x)
    def partialCompare(x: R, y: R): Double =
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

    def join(x: R, y: R): R = {
      val newSize: Int = x.size.max(y.size)
      val xSized = x.forSize(newSize).get
      val ySized = y.forSize(newSize).get
      val domain = Domain(newSize)
      implicit def aRepLattice: Lattice[aReps.R] = aReps.lattice
      val xPart = xSized.partition.inDomain(domain).get
      val yPart = ySized.partition.inDomain(domain).get
      val newPartition = xPart.join(yPart)
      val newRepForBlock = new Array[aReps.R](newPartition.numBlocks)
      var i = 0
      while (i < newPartition.numBlocks) {
        val block = newPartition.blocks(i)
        var newRep = aReps.lattice.zero
        var mutBlock = mutable.BitSet.empty ++= block
        // max by x
        while (mutBlock.nonEmpty) {
          val m = mutBlock.min
          newRep = newRep join x.repForIndex(m)
          mutBlock --= x.partition.blockFor(m)
        }
        mutBlock = mutable.BitSet.empty ++= block
        while(mutBlock.nonEmpty) {
          val m = mutBlock.min
          newRep = newRep join y.repForIndex(m)
          mutBlock --= y.partition.blockFor(m)
        }
        newRepForBlock(i) = newRep
      }
      R(newPartition, newRepForBlock)
    }
    def meet(x: R, y: R): R = {
      val workSize: Int = x.size.max(y.size)
      val newSize: Int = x.size.min(y.size)
      val xSized = x.forSize(newSize).get
      val ySized = y.forSize(newSize).get
      val workDomain = Domain(workSize)
      val newDomain = Domain(newSize)
      implicit def workPartitionLattice: BoundedLattice[workDomain.Partition] = workDomain.Partition.Algebra
      implicit def newPartitionLattice: BoundedLattice[newDomain.Partition] = newDomain.Partition.Algebra
      implicit def aRepLattice: Lattice[aReps.R] = aReps.lattice
      val xPart = xSized.partition.inDomain(workDomain).get
      val yPart = ySized.partition.inDomain(workDomain).get
      val workPartition = xPart meet yPart
      val newPartition = workPartition.inDomain(newDomain).get
      val newRepForBlock = new Array[aReps.R](newPartition.numBlocks)
      var i = 0
      while (i < newPartition.numBlocks) {
        val block = newPartition.blocks(i)
        var newRep: aReps.R = x.repForIndex(block.min)
        var mutBlock = mutable.BitSet.empty ++= block
        // max by x
        while (mutBlock.nonEmpty) {
          val m = mutBlock.min
          newRep = newRep meet x.repForIndex(m)
          mutBlock --= x.partition.blockFor(m)
        }
        mutBlock = mutable.BitSet.empty ++= block
        while(mutBlock.nonEmpty) {
          val m = mutBlock.min
          val yRep =
            newRep = newRep meet y.repForIndex(m)
          mutBlock --=  y.partition.blockFor(m)
        }
        newRepForBlock(i) = newRep
      }
      R(newPartition, newRepForBlock)
    }
  }
  trait RBuilder {
    def apply(partition: Domain#Partition, repForBlock: Array[aReps.R]): R
    def apply(wr: Wr[A, H]): R = {
      val size = wr.aSeq.size.max(wr.h.supportMax.getOrElse(0) + 1)
      val partition = Domain(size).Partition.fromPermutation(wr.h)
      val repForBlock = new Array[aReps.R](partition.numBlocks)
      var i = 0
      while (i < repForBlock.length) {
        val blockInSeq = partition.blocks(i).filter(_ < wr.aSeq.length)
        if (blockInSeq.isEmpty)
          repForBlock(i) = aReps.lattice.zero
        else
          repForBlock(i) = aReps.get(blockInSeq.map(wr.aSeq(_)))
        i += 1
      }
      apply(partition, repForBlock)
    }
  }
  type R <: ShapedR
  def R: RBuilder
  abstract class ShapedR extends Representation[Wr[A, H]] {
    selfR: R =>
    trait DefaultAction extends FaithfulPermutationAction[Wr[A, H]] {
      def supportMaxElement = size - 1
      def support(w: Wr[A, H]): Set[Int] =
        (0 until size).filter(k => actr(k, w) != k).toSet
      def supportMin(w: Wr[A, H]): NNOption = {
        val s = support(w)
        if (s.isEmpty) NNNone else NNSome(s.min)
      }
      def supportMax(w: Wr[A, H]): NNOption = {
        val s = support(w)
        if (s.isEmpty) NNNone else NNSome(s.max)
      }
      def actl(w: Wr[A, H], k: Int): Int = actr(k, w.inverse)
    }
    val representations = selfReps
    def partition: Domain#Partition
    def repForBlock: Array[aReps.R]
    def repForIndex(i: Int) = repForBlock(partition.blockIndex(i))
    def represents(w: Wr[A, H]): Boolean =
      if (w.aSeq.size > size) false else {
        val n = w.aSeq.size
        if (!partition.blocks.forall(_.forall( k => k > n || repForIndex(k).represents(w.aSeq(k)))))
          return false
        val domain = Domain(size)
        val myPartition: domain.Partition = partition.inDomain(domain).get
        val wPartition: domain.Partition = domain.Partition.fromPermutation(w.h)
        wPartition <= myPartition
      }
    def forSize(newSize: Int): RefOption[R] =
      if (newSize == partition.size)
        RefSome(this)
      else if (newSize > partition.size) {
        val newPartition = partition.inDomain(Domain(newSize)).getOrElse(sys.error("Partition enlargement always succeeds."))
        val minR = aReps.lattice.zero
        val newRepForBlock = new Array[aReps.R](newPartition.numBlocks)
        Array.copy(repForBlock, 0, newRepForBlock, 0, partition.numBlocks)
        var i = partition.numBlocks
        while (i < newPartition.numBlocks) {
          newRepForBlock(i) = minR
          i += 1
        }
        RefSome(R(newPartition, newRepForBlock))
      } else { // newSize < partition.size
        val newPartition = partition.inDomain(Domain(newSize)) match {
          case RefOption(np) => np
          case _ => return RefNone.asInstanceOf[RefOption[R]]
        }
        var i = newSize + 1
        val minR = aReps.lattice.zero
        while (i < partition.size) {
          if (repForBlock(partition.blockIndex(i)) != minR)
            return RefNone.asInstanceOf[RefOption[R]]
          i += 1
        }
        val newRepForBlock = new Array[aReps.R](newPartition.numBlocks)
        Array.copy(repForBlock, 0, newRepForBlock, 0, newPartition.numBlocks)
        RefSome(R(newPartition, newRepForBlock))
      }
  }
}
