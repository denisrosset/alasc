package net.alasc.laws
package generators

import scala.reflect.ClassTag
import scala.util.Random
import org.scalacheck.{Arbitrary, Gen}

import spire.syntax.group._
import spire.syntax.action._
import spire.syntax.cfor._
import spire.std.int._

import net.alasc.algebra._
import net.alasc.math._
import net.alasc.syntax.permutationAction._

object Domains {
  def sized: Gen[Domain] = Gen.posNum[Int].map(Domain(_))

  def partitionForDomain(domain: Domain): Gen[domain.Partition] =
    for {
      seq <- Gen.containerOfN[Seq, Int](domain.size, Gen.oneOf(0,1,2,3))
    } yield domain.Partition.fromSeq(seq)

  def partitionSized: Gen[Domain#Partition] =
    Domains.sized.flatMap(partitionForDomain(_))

  def partitionMapForDomain[V:ClassTag](domain: Domain, values: Gen[V]): Gen[domain.PartitionMap[V]] =
    for {
      partition <- partitionForDomain(domain)
      valueSeq <- Gen.containerOfN[Seq, V](partition.blocks.size, values)
    } yield domain.PartitionMap.tabulate(partition)(block => valueSeq(partition.blockIndex(block.min)))

  implicit def arbPartition(implicit domain: Domain): Arbitrary[domain.Partition] =
    Arbitrary(partitionForDomain(domain))
  implicit def arbPartitionMap[V:ClassTag:Arbitrary](implicit domain: Domain): Arbitrary[domain.PartitionMap[V]] =
    Arbitrary(partitionMapForDomain(domain, implicitly[Arbitrary[V]].arbitrary))
  implicit def arbDomain: Arbitrary[Domain] = Arbitrary(sized)

  object Projections {
    implicit val PartitionInstances = Instances(Seq(
      Domain.Partition.fromSeq(Seq(0,1,1)),
      Domain.Partition.fromSeq(Seq(0,0,1))
    ))
    implicit val PartitionCloner = Cloner((dp: Domain#Partition) => Domain.Partition(dp.blocks: _*))
    implicit def PartitionMapInstances[V:ClassTag:Instances]: Instances[Domain#PartitionMap[V]] =
      Instances(Instances[V].map(v => Domain.PartitionMap(Set(0, 1) -> v))) :+
    Domain.PartitionMap(Set(0) -> Instances[V].first, Set(1) -> Instances[V].second)

    implicit def PartitionMapCloner[V:ClassTag:Cloner]: Cloner[Domain#PartitionMap[V]] =
      Cloner((pm: Domain#PartitionMap[V]) => Domain.PartitionMap(pm.blocks.map {
        case (set, value) => (set, Cloner[V].make(value))
      }: _*))
    implicit def arbPartition: Arbitrary[Domain#Partition] = Arbitrary(partitionSized)
    implicit def arbPartitionMap[V:Arbitrary:ClassTag]: Arbitrary[Domain#PartitionMap[V]] =
      Arbitrary(Domains.sized.flatMap(
        partitionMapForDomain(_,
          implicitly[Arbitrary[V]].arbitrary
        )
      ))
  }
}
