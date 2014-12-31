package net.alasc.laws

import scala.reflect.ClassTag

import spire.algebra._
import spire.algebra.lattice._
import spire.math._

import spire.laws._

import scala.{ specialized => spec }

import org.typelevel.discipline.scalatest.Discipline
import org.scalacheck.{Gen, Arbitrary}

import org.scalatest.FunSuite

import net.alasc.algebra._
import net.alasc.math._
import net.alasc.math.wreath._

class LawTests extends FunSuite with Discipline {

  import AlascArbitrary._

  implicit def intArbitrary: Arbitrary[Int] = Arbitrary(Gen.choose(0, 100))

  implicit def basicAlgorithms: net.alasc.math.bsgs.algorithms.BasicAlgorithms[Perm] = Grp.defaultAlgorithms[Perm]
  val domain = Domain(32)
  implicit def domainArbitrary: Arbitrary[domain.Partition] = PartitionArbitrary(domain)
  implicit def intOrder: Order[Int] = spire.std.int.IntAlgebra
  implicit object intMinMaxLattice extends MinMaxLattice[Int] with BoundedLattice[Int]  {
    def zero = Int.MinValue
    def one = Int.MaxValue
  }
  implicit def partitionMapArbitrary[V : Arbitrary : ClassTag]: Arbitrary[domain.PartitionMap[V]] = PartitionMapArbitrary[V](domain)
  checkAll("Partition",                 LatticeLaws[domain.Partition].boundedLattice)
  checkAll("Partition",                 LatticeLaws[Domain#Partition].boundedBelowLattice)
  checkAll("domain.PartitionMap[Int]",  LatticeLaws[domain.PartitionMap[Int]].boundedLattice)
  implicit def partitionMapLattice = Domain.PartitionMapBoundedBelowLattice[Int]
  checkAll("Domain#PartitionMap[Int]",  LatticePartialOrderLaws[Domain#PartitionMap[Int]].boundedBelowLatticePartialOrder)
  checkAll("Grp[Perm]",                 LatticePartialOrderLaws[Grp[Perm]].boundedBelowLatticePartialOrder)
  checkAll("Perm",                      PermutationActionLaws[Perm].permutation)
  checkAll("Cycles",                    PermutationActionLaws[Cycles].permutation)

  {
    implicit def action = {
      val wrir = new WrImprimitiveRepresentations[Perm, Perm]
      val permR = wrir.aReps.get(Iterable(Perm(0, wrPermSize - 1)))
      wrir.R(wrSize, permR).action
    }
    checkAll("Wr[Perm, Perm] (imprimitive)",          PermutationActionLaws[Wr[Perm, Perm]].faithfulPermutationAction)
  }
  {
    implicit def action = {
      val wrir = new WrPrimitiveRepresentations[Perm, Perm]
      val permR = wrir.aReps.get(Iterable(Perm(0, wrPermSize - 1)))
      wrir.R(wrSize, permR).action
    }
    checkAll("Wr[Perm, Perm] (primitive)",            PermutationActionLaws[Wr[Perm, Perm]].faithfulPermutationAction)
  }
  {
    implicit def action = {
      val wrir = new InhWrImprimitiveRepresentations[Perm, Perm]
      implicit def ct = wrir.aReps.rClassTag
      val permR = wrir.aReps.get(Iterable(Perm(0, wrPermSize - 1)))
      val domain = Domain(wrSize)
      val partition = domain.Partition((0 until wrSize).map(Set(_)):_*)
      wrir.R(partition, Array.fill(wrSize)(permR)).action
    }
    checkAll("Wr[Perm, Perm] (inh imprimitive)",      PermutationActionLaws[Wr[Perm, Perm]].faithfulPermutationAction)
  }
  {
    implicit def action = {
      val wrir = new InhWrPrimitiveRepresentations[Perm, Perm]
      implicit def ct = wrir.aReps.rClassTag
      val permR = wrir.aReps.get(Iterable(Perm(0, wrPermSize - 1)))
      val domain = Domain(wrSize)
      val partition = domain.Partition((0 until wrSize).map(Set(_)):_*)
      wrir.R(partition, Array.fill(wrSize)(permR)).action
    }
    checkAll("Wr[Perm, Perm] (inh primitive)",        PermutationActionLaws[Wr[Perm, Perm]].faithfulPermutationAction)
  }
  {
    val wrir: InhWrImprimitiveRepresentations[Perm, Perm] = new InhWrImprimitiveRepresentations[Perm, Perm]
    implicit def partialOrder: PartialOrder[wrir.R] = wrir.partialOrder
    implicit def lattice: Lattice[wrir.R] with BoundedJoinSemilattice[wrir.R] = wrir.lattice
    implicit def latticeElement = Arbitrary { genWrPermPerm.map(w => wrir.get(Iterable(w))) }
    checkAll("InhWrImprimitiveRepresentations[Perm, Perm].lattice",
      LatticePartialOrderLaws[wrir.R].boundedBelowLatticePartialOrder)
  }
  // checkAll Hash Partition / PartitionMap
}
