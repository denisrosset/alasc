package net.alasc.laws

import scala.reflect.ClassTag

import spire.algebra._
import spire.algebra.lattice._
import spire.math._
import spire.implicits.{
  SeqOrder => _, SeqEq => _,
  ArrayOrder => _, ArrayEq => _,
  MapEq => _,
  _ }

import spire.laws._

import scala.{ specialized => spec }

import org.typelevel.discipline.scalatest.Discipline
import org.scalacheck.{Gen, Arbitrary}

import org.scalatest.FunSuite

import net.alasc.math._

class LawTests extends FunSuite with Discipline {

  import AlascArbitrary._

  implicit def basicAlgorithms: net.alasc.math.bsgs.algorithms.BasicAlgorithms[Perm] = Grp.defaultAlgorithms[Perm]
//  checkAll("Grp[Perm]",           LatticePartialOrderLaws[Grp[Perm]].boundedBelowLatticePartialOrder(Grp.lattice[Perm]))
  val domain = Domain(32)
  implicit def domainArbitrary: Arbitrary[domain.Partition] = PartitionArbitrary(domain)
  object intMinMaxLattice extends MinMaxLattice[Int] with BoundedLattice[Int] with spire.std.IntOrder {
    def zero = Int.MinValue
    def one = Int.MaxValue
  }
  implicit def intArbitrary: Arbitrary[Int] = Arbitrary(Gen.choose(0, 100))
  implicit def partitionMapArbitrary[V : Arbitrary : ClassTag]: Arbitrary[domain.PartitionMap[V]] = PartitionMapArbitrary[V](domain)
  checkAll("Partition",           LatticeLaws[domain.Partition].boundedLattice)
  checkAll("Partition",           LatticeLaws[Domain#Partition].boundedBelowLattice)
  checkAll("PartitionMap",        LatticeLaws[domain.PartitionMap[Int]].boundedLattice)
  checkAll("PartitionMap",        LatticeLaws[Domain#PartitionMap[Int]].boundedBelowLattice)
  checkAll("PartitionMap",        LatticePartialOrderLaws[Domain#PartitionMap[Int]].boundedBelowLatticePartialOrder)
}
