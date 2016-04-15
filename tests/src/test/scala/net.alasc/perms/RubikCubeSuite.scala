package net.alasc.perms

import org.scalatest.{FunSuite, Matchers}

import net.alasc.domains._
import net.alasc.prep._
import net.alasc.named.RubikCube

// http://www.gap-system.org/Doc/Examples/rubik.html
class RubikCubeSuite extends FunSuite with Matchers {

  import RubikCube.{order, colors}

  def testGroupOrder()(implicit builder: PGrpBuilder[Perm]): Unit = {
    val grp = RubikCube[Perm]
    grp.order shouldBe order
  }

  def testMovesFaces()(implicit builder: PGrpBuilder[Perm]): Unit = {
    val grp = RubikCube[Perm]
    val partition = Partition.fromSeq(colors)
    grp.fixingPartition(partition).isTrivial shouldBe true
  }

  test("Rubik cube group has correct order / deterministic") {    
    import PGrp.deterministic._
    testGroupOrder()
  }

  test("Rubik cube group has correct order / randomized") {
    import PGrp.default._
    testGroupOrder()
  }

  test("Rubik cube group moves the faces / deterministic") {
    import PGrp.deterministic._
    testMovesFaces()
  }

  test("Rubik cube group moves the faces / randomized") {
    import PGrp.default._
    testMovesFaces()
  }

}
