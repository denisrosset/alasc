package net.alasc.tests
package perms

import org.scalacheck._

import net.alasc.partitions._
import net.alasc.finite.Grp
import net.alasc.named.Symmetric
import net.alasc.perms.{GrpFixingPartition, Perm}

final class FixingPartitionSuite extends AlascSuite {

  import net.alasc.perms.default._

  val genSeq = for {
    n <- Gen.choose(1, 30)
    seq <- Gen.containerOfN[Seq, Int](n, Gen.choose(0, 4))
  } yield seq

  implicit val noShrinkSeq = noShrink[Seq[Int]]

  test("creation") {
    forAll(genSeq) { seq =>
      val partition = Partition.fromSeq(seq)
      val subgroup1 = GrpFixingPartition(Partition.fromSeq(seq))
      val subgroup2 = Symmetric(seq.size).fixingPartition(partition)
      (subgroup1: Grp[Perm]) should === (subgroup2: Grp[Perm])
      subgroup1.generators.forall(g => (seq <|+|? g).get.sameElements(seq)) shouldBe true
    }
  }

}
