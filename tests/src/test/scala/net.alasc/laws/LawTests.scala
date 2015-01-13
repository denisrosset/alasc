package net.alasc.laws

import scala.reflect.ClassTag

import spire.algebra._
import spire.algebra.lattice._
import spire.math._
import spire.laws._

import scala.{ specialized => spec }

import org.typelevel.discipline.scalatest.Discipline
import org.scalacheck.{Gen, Arbitrary}
  import spire.std.int.IntAlgebra

import org.scalatest.FunSuite

import net.alasc.algebra._
import net.alasc.math._
import net.alasc.math.wreath._

class LawTests extends FunSuite with NestedDiscipline {
  implicit def intArbitrary: Arbitrary[Int] =
    Arbitrary(Gen.choose(Int.MinValue, Int.MaxValue))
  import Domains.{arbPartition, arbPartitionMap, arbDomain}

  nestedCheckAll[Domain]("Domain.Partition", Domain(1)) { implicit domain =>
    LatticeLaws[domain.Partition].boundedLattice
  }
  implicit object intMinMaxLattice extends MinMaxLattice[Int] with BoundedLattice[Int]  {
    def zero = Int.MinValue
    def one = Int.MaxValue
  }
  nestedCheckAll[Domain]("Domain.PartitionMap[Int]", Domain(1)) { implicit domain =>
    LatticeLaws[domain.PartitionMap[Int]].boundedLattice
  }

  // TODO use implicit trait priority
  implicit def basicAlgorithms: net.alasc.math.bsgs.algorithms.BasicAlgorithms[Perm] = Grp.defaultAlgorithms[Perm]
  import Permutations.{arbPermutation, arbDom}
  import Grps.arbGrp
  import Wrs.arbWr

  checkAll("Grp[Perm]", LatticePartialOrderLaws[Grp[Perm]].boundedBelowLatticePartialOrder)
  checkAll("Perm",      PermutationActionLaws[Perm].permutation)
  checkAll("Cycles",    PermutationActionLaws[Cycles].permutation)

  {
    import Domains.Projections.{arbPartition, arbPartitionMap}
    import Domains.Projections._
    implicit def partitionMapLattice = Domain.PartitionMapBoundedBelowLattice[Int]

    checkAll("Domain#Partition", AnyRefLaws[Domain#Partition]._eq)
    checkAll("Domain#PartitionMap[Int]", AnyRefLaws[Domain#PartitionMap[Int]]._eq)
    checkAll("Domain#PartitionMap[Int]",  LatticePartialOrderLaws[Domain#PartitionMap[Int]].boundedBelowLatticePartialOrder)
  }

  nestedCheckAll[WrSize]("Wr[Perm,Perm] (imprimitive)", WrSize(1, 1)) { implicit wrSize =>
    implicit def action = wrSize.imprimitiveRepresentation[Perm, Perm].action
    PermutationActionLaws[Wr[Perm, Perm]].faithfulPermutationAction
  }

  {
    implicit def arbWrSize = WrSize.arbWrSizeForPrimitive
    nestedCheckAll[WrSize]("Wr[Perm,Perm] (primitive)", WrSize(2, 2)) { implicit wrSize =>
      implicit def action = wrSize.primitiveRepresentation[Perm, Perm].action
      PermutationActionLaws[Wr[Perm, Perm]].faithfulPermutationAction
    }
  }

/*
  /*
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
   */
 */
}
