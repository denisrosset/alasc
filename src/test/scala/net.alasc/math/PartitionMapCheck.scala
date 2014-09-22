package net.alasc.math

import org.scalacheck._
import org.scalacheck.Arbitrary._
import org.scalatest._
import prop._
import spire.syntax.eq._
import net.alasc.algebra._

object NonNegIntLattice extends BoundedBelowLattice[Int] {
  def zero = 0
  def join(x: Int, y: Int) = x.max(y)
  def meet(x: Int, y: Int) = x.min(y)
  def partialCompare(x: Int, y: Int): Double = ((x - y).signum).toDouble
}

class NonNegIntLatticeCheck extends BoundedBelowLatticeCheck[Int] {
  implicit def lattice = NonNegIntLattice
  implicit def arbitraryLatticeElement = Arbitrary {
    Gen.choose(0, 1000)
  }
}

class PartitionMapCheck extends BoundedBelowLatticeCheck[PartitionMap[Int]] {
  implicit def intLattice: BoundedBelowLattice[Int] = NonNegIntLattice
  implicit def lattice = PartitionMap.PartitionMapLattice[Int]
  implicit def arbitraryLatticeElement = Arbitrary {
    for {
      size <- Gen.choose(1, 15)
      seq <- Gen.containerOfN[Seq, Int](size, Gen.choose(0, 4))
    } yield PartitionMap.tabulate(Domain(size).Partition.fromSeq(seq))( block => seq(block.min) )
  }
}
