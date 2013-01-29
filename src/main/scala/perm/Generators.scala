package com.faacets.perm

object Generators {
  def partitionPreserving(partition: Iterable[Seq[Int]]): Iterable[ExplicitPermutation] = {
    val domainSize = partition.flatten.max + 1
    def genForPoints(points: Seq[Int]): Seq[ExplicitPermutation] =
      (points zip points.tail).map( p => ExplicitPermutation(domainSize)(p._1,p._2) )
    partition.map(genForPoints(_)).flatten
  }
  def symmetryGroup(degree: Int): Seq[ExplicitPermutation] =
    (0 to degree - 2).map(i => ExplicitPermutation(degree)(i, i + 1))
}
