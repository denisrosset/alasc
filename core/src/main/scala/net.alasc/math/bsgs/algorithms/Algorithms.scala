package net.alasc.math
package bsgs
package algorithms

import scala.reflect.ClassTag
import scala.util.Random

import spire.algebra.{Eq, Group}

trait Algorithms[P] {
  implicit def group: Group[P]
  implicit def equ: Eq[P]
  implicit def classTag: ClassTag[P]
}

trait BasicAlgorithms[P] extends SchreierSims[P] with BaseChange[P] with BaseSwap[P] with BaseAlgorithms[P] with SubgroupSearch[P] with SchreierSimsCommon[P] with ChainBuilder[P]

object BasicAlgorithms {
  def deterministic[P:ClassTag:Eq:Group]: BasicAlgorithms[P] =
    new BasicAlgorithms[P] with BaseSwapDeterministic[P] with BaseChangeSwapConjugation[P] with SchreierSimsDeterministic[P] with SubgroupSearchImpl[P] {
      val classTag = implicitly[ClassTag[P]]
      val equ = implicitly[Eq[P]]
      val group = Group[P]
      val nodeBuilder = new MutableNodeExplicitBuilder[P]
    }
  def randomized[P:ClassTag:Eq:Group](random: Random = Random): BasicAlgorithms[P] =
    new BasicAlgorithms[P] with RandomizedAlgorithms with BaseSwapRandomized[P] with BaseChangeSwapConjugation[P] with SchreierSimsRandomized[P] with SubgroupSearchImpl[P] {
      val classTag = implicitly[ClassTag[P]]
      val equ = implicitly[Eq[P]]
      val group = Group[P]
      implicit val randomGenerator = random
      val nodeBuilder = new MutableNodeExplicitBuilder[P]
    }
}

trait RandomizedAlgorithms {
  def randomGenerator: Random
}
