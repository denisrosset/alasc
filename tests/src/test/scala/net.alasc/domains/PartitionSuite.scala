package net.alasc.domains

import scala.collection.SortedSet

import org.scalatest.{FunSuite, Matchers}

class PartitionSuite extends FunSuite with Matchers {

  test("Partition bug") {
    val t1: Partition = Partition.fromSortedBlocks(Domain(8))(Seq(SortedSet(0,5,6,2,4),SortedSet(1,3,7)))
    val t2 = Partition(Set(0,5,6,2,4),Set(1,3,7))
    t1.toString == t2.toString
  }

}
