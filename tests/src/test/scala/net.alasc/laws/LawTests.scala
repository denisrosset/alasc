package net.alasc.laws

import spire.algebra.lattice._
import spire.laws.{Perm => _, _}

import org.scalacheck.{Gen, Arbitrary}
import spire.std.int.IntAlgebra

import org.scalatest.FunSuite

import net.alasc.domains._
import net.alasc.finite._
import net.alasc.perms._

class LawTests extends FunSuite with NestedDiscipline {

  implicit def intArbitrary: Arbitrary[Int] =
    Arbitrary(Gen.choose(Int.MinValue, Int.MaxValue))

  import Domains.arbDomain
  import Partitions.{arbPartition, arbPartitionIn}
  import PartitionMaps.{arbPartitionMap, arbPartitionMapIn}

  nestedCheckAll[Domain]("Domain.Partition", Domain(1)) { d =>
    val domain: Domain = d
    LatticeLaws[Partition.In[domain.type]].boundedLattice
  }

  implicit object intMinMaxLattice extends MinMaxLattice[Int] with BoundedLattice[Int]  {
    def zero = Int.MinValue
    def one = Int.MaxValue
  }

  nestedCheckAll[Domain]("Domain.PartitionMap[Int]", Domain(1)) { d =>
    val domain: Domain = d
    LatticeLaws[PartitionMap.In[domain.type, Int]].boundedLattice
  }

  // TODO use implicit trait priority
  import net.alasc.prep.PGrp.deterministic._
  import Permutations.{arbPermutation, arbDom, permutationInstances, permutationCloner}
  import Grps.{arbGrp, arbPGrp}

  checkAll("Grp[Perm]", PGrpLaws[Perm].grp)
  checkAll("Grp[Perm]", LatticePartialOrderLaws[Grp[Perm]].boundedBelowLatticePartialOrder)
  checkAll("Perm",      PermutationActionLaws[Perm].permutation)
  checkAll("Cycles",    PermutationActionLaws[Cycles].permutation)
  checkAll("Perm",      AnyRefLaws[Perm]._eq)
  checkAll("Cycles",    AnyRefLaws[Perm]._eq)

  {
    import Partitions.{arbPartition, partitionInstances, partitionCloner}
    import PartitionMaps.{arbPartitionMap, partitionMapInstances, partitionMapCloner}
    import Domains.arbDomain

    implicit def partitionMapLattice = PartitionMap.boundedBelowLattice[Int]

    checkAll("Partition", AnyRefLaws[Partition]._eq)
    checkAll("PartitionMap[Int]", AnyRefLaws[PartitionMap[Int]]._eq)
    checkAll("PartitionMap[Int]",  LatticePartialOrderLaws[PartitionMap[Int]].boundedBelowLatticePartialOrder)
  }

  {
    import Wrs.arbWr
    import net.alasc.wreath._
    nestedCheckAll[WrSize]("Wr[Perm,Perm] (imprimitive)", WrSize(1, 1)) { implicit wrSize =>
      implicit def action = wrSize.imprimitiveRepresentation[Perm, Perm].permutationAction
      PermutationActionLaws[Wr[Perm, Perm]].faithfulPermutationAction
    }
  }
  {
    import Wrs.arbWr
    import net.alasc.wreath._
    implicit def arbWrSize = WrSize.arbWrSizeForPrimitive
    nestedCheckAll[WrSize]("Wr[Perm,Perm] (primitive)", WrSize(2, 2)) { implicit wrSize =>
      implicit def action = wrSize.primitiveRepresentation[Perm, Perm].permutationAction
      PermutationActionLaws[Wr[Perm, Perm]].faithfulPermutationAction
    }
  }

}
