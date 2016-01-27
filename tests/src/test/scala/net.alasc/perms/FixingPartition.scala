package net.alasc.perms

import scala.util.Random

import org.scalacheck._
import org.scalatest.FunSuite

import spire.syntax.partialAction._

import net.alasc.algebra._
import net.alasc.domains._
import net.alasc.named.Symmetric
import net.alasc.prep._
import net.alasc.std.seq._
import net.alasc.syntax.all._
import net.alasc.laws._

object FixingPartitionCheck extends Properties("FixingPartition") {

  import PGrp.default._

  val genSeq = for {
    n <- Gen.choose(1, 30)
    seq <- Gen.containerOfN[Seq, Int](n, Gen.choose(0, 4))
  } yield seq

  property("creation") = Prop.forAllNoShrink(genSeq) { seq =>
    val partition = Partition.fromSeq(seq)
    val subgroup1 = FixingPartition[Perm](Partition.fromSeq(seq))
    val subgroup2 = Symmetric[Perm](seq.size).fixingPartition(partition)
    (subgroup1 == subgroup2) &&
    subgroup1.generators.forall( g => (seq <|+|? g).get.sameElements(seq) )
  }

}
