package net.alasc.laws

import scala.reflect.ClassTag
import org.scalacheck.{Arbitrary, Gen}
import Arbitrary.arbitrary

import net.alasc.partitions._

object PartitionMaps {

  def forSize[V:ClassTag](size: Int, values: Gen[V]): Gen[PartitionMap[V]] =
    for {
      partition <- Partitions.forSize(size)
      valueSeq <- Gen.containerOfN[Seq, V](partition.blocks.size, values)
    } yield PartitionMap.tabulate(partition)(block => valueSeq(partition.blockIndex(block.min)))

  implicit def arbPartitionMap[V:Arbitrary:ClassTag]: Arbitrary[PartitionMap[V]] =
    Arbitrary { Gen.size.flatMap[PartitionMap[V]] { s => PartitionMaps.forSize(s, arbitrary[V]) } }

  implicit def partitionMapInstances[V:ClassTag:Instances]: Instances[PartitionMap[V]] =
    Instances(Instances[V].map(v => PartitionMap(Set(0, 1) -> v))) :+
  PartitionMap(Set(0) -> Instances[V].first, Set(1) -> Instances[V].second)

  implicit def partitionMapCloner[V:ClassTag:Cloner]: Cloner[PartitionMap[V]] =
    Cloner((pm: PartitionMap[V]) => PartitionMap(pm.blocks.map {
      case (set, value) => (set, Cloner[V].make(value))
    }: _*))

}
