package net.alasc.laws

import org.scalacheck.{Arbitrary, Gen}

import net.alasc.domains._

abstract class PartitionsLowerPriority {

  implicit def arbPartition: Arbitrary[Partition] = Arbitrary(Partitions.sized)

}

object Partitions extends PartitionsLowerPriority {

  def forDomain(domain: Domain): Gen[Partition.In[domain.type]] =
    Gen.containerOfN[Seq, Int](domain.size, Gen.choose(0, 3))
      .map( seq => Partition.fromSeq(domain)(seq) )

  def sized: Gen[Partition] = Domains.sized.flatMap[Partition](forDomain(_))

  implicit def arbPartitionIn[D <: Domain with Singleton](implicit witness: shapeless.Witness.Aux[D]): Arbitrary[Partition.In[D]] =
    Arbitrary(forDomain(witness.value: D))

  implicit val partitionInstances: Instances[Partition] = Instances(Seq(
    Partition.fromSeq(Seq(0,1,1)),
    Partition.fromSeq(Seq(0,0,1))
  ))

  implicit val partitionCloner = Cloner((dp: Partition) => Partition(dp.blocks: _*))

}
