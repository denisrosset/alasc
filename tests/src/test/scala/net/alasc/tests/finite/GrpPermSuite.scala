package net.alasc.tests
package finite

import net.alasc.bsgs.PartitionStabilizer
import spire.laws.LatticePartialOrderLaws
import net.alasc.finite.{Grp, GrpGroup}
import net.alasc.laws._
import net.alasc.partitions.Partition
import net.alasc.perms.Perm
import net.alasc.wreath.{Wr, WrFaithfulPermutationAction}

class GrpPermSuite extends AlascSuite {

  import Permutations.arbPerm
  import Grps.arbGrp


  {
    import net.alasc.perms.default._
    checkAll("Grp[Perm] with permutation action", GrpLaws[Perm].grpPermutationAction)

    checkAll("Group lattice laws", LatticePartialOrderLaws[Grp[Perm]].boundedBelowLatticePartialOrder)
  }

  test("Partition stabilizer test case") {
    val g1 = Wr(0 -> Perm(0,2), 1 -> Perm(0,2))(0,2,1)
    val g2 = Wr(0 -> Perm(0,1), 1 -> Perm(1,2))(0,1)
    val g3 = Wr(0 -> Perm(0,2), 1 -> Perm(0,2,1))(1,2)
    val partition = Partition(Set(0), Set(1), Set(2, 3))
    val action = new WrFaithfulPermutationAction[Perm](3, 3)
    val h1 = action.toPerm(g1)
    val h2 = action.toPerm(g2)
    val h3 = action.toPerm(g3)
    val sub1 = {
      import net.alasc.perms.default._
      Grp(h1,h2,h3).partitionStabilizer(partition)
    }
    val sub2 = {
      import net.alasc.blackbox._
      Grp(h1,h2,h3).partitionStabilizer(partition)
    }
    (sub1 === sub2) shouldBe true
  }
}
