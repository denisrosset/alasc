package net.alasc.tests
package perms

import net.alasc.domains._
import net.alasc.named.RubikCube
import net.alasc.perms.{Perm, PermGrpBuilder}

// http://www.gap-system.org/Doc/Examples/rubik.html
abstract class RubikCubeSuite(implicit builder: PermGrpBuilder[Perm]) extends AlascSuite {

  import RubikCube.{colors, order}

  test("Rubik cube group has correct order") {
    val grp = RubikCube[Perm]
    grp.order shouldBe order
  }

  test("Rubik cube group moves the faces") {
    val grp = RubikCube[Perm]
    val partition = Partition.fromSeq(colors)
    grp.fixingPartition(partition).isTrivial shouldBe true
  }

}

class RubikCubeSuiteDeterministic extends RubikCubeSuite()(PermSuite.deterministic)

class RubikCubeSuiteRandomized extends RubikCubeSuite()(PermSuite.randomized)