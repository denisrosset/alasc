package net.alasc.laws

import org.scalacheck.{Arbitrary, Gen}

import net.alasc.partitions._

object Partitions {

  def forSize(size: Int): Gen[Partition] =
    Gen.containerOfN[Seq, Int](size, Gen.choose(0, 3))
      .map( seq => Partition.fromSeq(seq) )

  def sized: Gen[Partition] = Gen.posNum[Int].flatMap(forSize)

  implicit def arbPartition: Arbitrary[Partition] = Arbitrary(Partitions.sized)

  implicit val partitionInstances: Instances[Partition] = Instances(Seq(
    Partition.fromSeq(Seq(0,1,1)),
    Partition.fromSeq(Seq(0,0,1))
  ))

  implicit val partitionCloner = Cloner((dp: Partition) => Partition(dp.blocks: _*))

}
