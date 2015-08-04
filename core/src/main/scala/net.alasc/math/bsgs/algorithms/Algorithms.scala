package net.alasc.math
package bsgs
package algorithms

import scala.reflect.ClassTag
import scala.util.Random

import spire.algebra.Eq

import net.alasc.algebra.FiniteGroup

trait Algorithms[P] {
  implicit def finiteGroup: FiniteGroup[P]
  implicit def equality: Eq[P]
  implicit def classTag: ClassTag[P]
}

trait BasicAlgorithms[P] extends SchreierSims[P] with BaseChange[P] with BaseSwap[P] with BaseAlgorithms[P] with SubgroupSearch[P] with SchreierSimsCommon[P] with ChainBuilder[P]

object BasicAlgorithms {
  def deterministic[P: ClassTag: Eq: FiniteGroup]: BasicAlgorithms[P] =
    new BasicAlgorithms[P] with BaseSwapDeterministic[P] with BaseChangeSwapConjugation[P] with SchreierSimsDeterministic[P] with SubgroupSearchImpl[P] {
      val classTag = implicitly[ClassTag[P]]
      val equality = implicitly[Eq[P]]
      val finiteGroup = FiniteGroup[P]
      val nodeBuilder = new MutableNodeExplicitBuilder[P]
    }
  def randomized[P: ClassTag: Eq: FiniteGroup](random: Random = Random): BasicAlgorithms[P] =
    new BasicAlgorithms[P] with RandomizedAlgorithms with BaseSwapRandomized[P] with BaseChangeSwapConjugation[P] with SchreierSimsRandomized[P] with SubgroupSearchImpl[P] {
      val classTag = implicitly[ClassTag[P]]
      val equality = implicitly[Eq[P]]
      val finiteGroup = FiniteGroup[P]
      implicit val randomGenerator = random
      val nodeBuilder = new MutableNodeExplicitBuilder[P]
    }
}

trait RandomizedAlgorithms {
  def randomGenerator: Random
}
