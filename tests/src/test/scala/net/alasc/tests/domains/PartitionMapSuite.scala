package net.alasc.tests
package domains

import spire.algebra.lattice.{BoundedLattice, MinMaxLattice}
import spire.laws.{LatticeLaws, LatticePartialOrderLaws}

import org.scalacheck.{Arbitrary, Gen}

import net.alasc.domains.{Domain, PartitionMap}
import net.alasc.laws.{AnyRefLaws, Domains, PartitionMaps}

class PartitionMapSuite extends AlascSuite {

  import Domains.arbDomain


  implicit def intArbitrary: Arbitrary[Int] =
    Arbitrary(Gen.choose(Int.MinValue, Int.MaxValue))

  implicit object intMinMaxLattice extends MinMaxLattice[Int] with BoundedLattice[Int]  {
    def zero = Int.MinValue
    def one = Int.MaxValue
  }

  nestedCheckAll[Domain]("Domain.PartitionMap[Int]", Domain(1)) { d =>
    import PartitionMaps.arbPartitionMapIn
    val domain: Domain = d
    LatticeLaws[PartitionMap.In[domain.type, Int]].boundedLattice
  }

  {
    import PartitionMaps.{arbPartitionMap, partitionMapCloner, partitionMapInstances}
    implicit def partitionMapLattice = PartitionMap.boundedBelowLattice[Int]
    checkAll("PartitionMap[Int]", AnyRefLaws[PartitionMap[Int]]._eq)
    checkAll("PartitionMap[Int]",  LatticePartialOrderLaws[PartitionMap[Int]].boundedBelowLatticePartialOrder)
  }

}
