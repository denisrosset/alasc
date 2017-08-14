package net.alasc.tests
package perms

import net.alasc.partitions._
import net.alasc.finite.{GrpGroup, GrpPermutationAction}
import net.alasc.named.RubikCube
import net.alasc.perms.Perm

// http://www.gap-system.org/Doc/Examples/rubik.html
abstract class RubikCubeSuite(implicit gg: GrpGroup[Perm], gpa: GrpPermutationAction[Perm]) extends AlascSuite {

  import RubikCube.{colors, order}

  test("Rubik cube group has correct order") {
    val grp = RubikCube()
    grp.order shouldBe order
  }

  test("Rubik cube group moves the faces") {
    val grp = RubikCube()
    val partition = Partition.fromSeq(colors)
    grp.fixingPartition(partition).isTrivial shouldBe true
  }

}

class RubikCubeSuiteDeterministic extends RubikCubeSuite()(PermSuite.deterministic, PermSuite.deterministic)

class RubikCubeSuiteRandomized extends RubikCubeSuite()(PermSuite.randomized, PermSuite.randomized)