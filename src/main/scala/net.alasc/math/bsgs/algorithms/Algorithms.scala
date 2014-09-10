package net.alasc.math
package bsgs
package algorithms

import scala.util.Random

import net.alasc.algebra.FiniteGroup

trait Algorithms[P] {
  implicit def algebra: FiniteGroup[P]
}

trait BasicAlgorithms[P] extends SchreierSims[P] with BaseChange[P] with BaseSwap[P] with BaseAlgorithms[P] with Orders[P] with SubgroupSearch[P] with SchreierSimsCommon[P] with ChainBuilder[P]

object BasicAlgorithms {
  def deterministic[P](implicit givenAlgebra: FiniteGroup[P]): BasicAlgorithms[P] =
    new BasicAlgorithms[P] with BaseSwapDeterministic[P] with BaseChangeSwapConjugation[P] with SchreierSimsDeterministic[P] with OrdersImpl[P] with SubgroupSearchImpl[P] {
      implicit val algebra = givenAlgebra
      implicit val nodeBuilder = new MutableNodeExplicitBuilder[P]
    }
  def randomized[P](random: Random = Random)(implicit givenAlgebra: FiniteGroup[P]): BasicAlgorithms[P] =
    new BasicAlgorithms[P] with RandomizedAlgorithms with BaseSwapRandomized[P] with BaseChangeSwapConjugation[P] with SchreierSimsRandomized[P] with OrdersImpl[P] with SubgroupSearchImpl[P] {
      implicit val algebra = givenAlgebra
      implicit val nodeBuilder = new MutableNodeExplicitBuilder[P]
      implicit val randomGenerator = random
    }
}

trait RandomizedAlgorithms {
  def randomGenerator: Random
}
