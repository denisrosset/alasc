package net.alasc.math

import org.scalacheck._
import org.scalacheck.Arbitrary._
import org.scalatest._
import prop._
import spire.syntax.eq._
import net.alasc.algebra._

object PartitionCheck {
  val domain = Domain(15)
  def genPartition = for {
    seq <- Gen.containerOfN[Seq, Int](domain.size, Gen.choose(0, 4))
  } yield domain.Partition.fromSeq(seq)
}

class PartitionCheck extends BoundedLatticeCheck[PartitionCheck.domain.Partition] {
  import PartitionCheck.domain
  implicit def lattice = domain.Partition.Algebra
  implicit def arbitraryLatticeElement = Arbitrary { PartitionCheck.genPartition }
}

class PartitionHashCheck extends HashCheck[PartitionCheck.domain.Partition] {
  import PartitionCheck.domain
  implicit def arb = Arbitrary { PartitionCheck.genPartition }
  implicit def eq = domain.Partition.Algebra
  def clone(h: domain.Partition) = domain.Partition.fromSeq(h.blockIndex)
}
