package net.alasc.laws

import scala.reflect.ClassTag
import scala.util.Random
import org.scalacheck.{Arbitrary, Gen}

import spire.syntax.group._
import spire.syntax.action._
import spire.syntax.cfor._
import spire.std.int._

import net.alasc.algebra._
import net.alasc.domains._
import net.alasc.syntax.permutationAction._

abstract class PartitionMapsLowerPriority {

  implicit def arbPartitionMap[V:Arbitrary:ClassTag]: Arbitrary[PartitionMap[V]] =
    Arbitrary(Domains.sized.flatMap[PartitionMap[V]] {
      domain => PartitionMaps.forDomain(domain)(implicitly[Arbitrary[V]].arbitrary)
    })

}

object PartitionMaps extends PartitionMapsLowerPriority {

  def forDomain[V:ClassTag](domain: Domain)(values: Gen[V]): Gen[PartitionMap.In[domain.type, V]] =
    for {
      partition <- Partitions.forDomain(domain)
      valueSeq <- Gen.containerOfN[Seq, V](partition.blocks.size, values)
    } yield PartitionMap.tabulate(partition: Partition.In[domain.type])(block => valueSeq(partition.blockIndex(block.min)))

  implicit def arbPartitionMapIn[D <: Domain with Singleton, V:ClassTag:Arbitrary](implicit witness: shapeless.Witness.Aux[D]): Arbitrary[PartitionMap.In[D, V]] =
    Arbitrary(forDomain(witness.value: D)(implicitly[Arbitrary[V]].arbitrary))

  implicit def partitionMapInstances[V:ClassTag:Instances]: Instances[PartitionMap[V]] =
    Instances(Instances[V].map(v => PartitionMap(Set(0, 1) -> v))) :+
  PartitionMap(Set(0) -> Instances[V].first, Set(1) -> Instances[V].second)


  implicit def partitionMapCloner[V:ClassTag:Cloner]: Cloner[PartitionMap[V]] =
    Cloner((pm: PartitionMap[V]) => PartitionMap(pm.blocks.map {
      case (set, value) => (set, Cloner[V].make(value))
    }: _*))

}
