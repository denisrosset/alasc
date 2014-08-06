package net.alasc.math
package bsgs

import scala.util.Random
import scala.collection.immutable.IntMap
import scala.collection.mutable.{BitSet => MutableBitSet}
import scala.annotation.tailrec
import spire.syntax.groupAction._
import spire.syntax.group._
import net.alasc.algebra.Permutation

class TransversalExplicit[P] private[bsgs](val beta: Int, val intMap: IntMap[InversePair[P]])(implicit val algebra: Permutation[P]) extends Transversal[P] {
  def builder = TransversalExplicit

  override def size = intMap.size
  def isDefinedAt(b: Int) = intMap.isDefinedAt(b)
  def apply(b: Int) = intMap.apply(b)
  override def get(b: Int): Option[InversePair[P]] = intMap.get(b)
  def iterator = intMap.iterator
  override def keysIterator = intMap.keysIterator
  override def valuesIterator = intMap.valuesIterator

  def conjugatedBy(ip: InversePair[P]): TransversalExplicit[P] = {
    val InversePair(f, fInv) = ip
    val newMap: IntMap[InversePair[P]] = intMap.map {
      case (b, InversePair(v, vInv)) => ((b <|+| f) -> InversePair(fInv |+| v |+| f, fInv |+| vInv |+| f))
    }
    new TransversalExplicit(beta <|+| f, newMap)
  }

  def mapValues[Q: Permutation](f: P => Q): TransversalExplicit[Q] =
    new TransversalExplicit(beta, IntMap.empty ++ intMap.mapValues( ip => InversePair(f(ip.g), f(ip.gInv)) ))

  def updatedPair(newGenerators: Iterable[InversePair[P]], allGenerators: Iterable[InversePair[P]]): TransversalExplicit[P] = {
    if (newGenerators.isEmpty) return this
    var newMap = intMap
    var toCheck = MutableBitSet.empty
    for (ip@InversePair(g, gInv) <- newGenerators; b <- intMap.keysIterator) {
      val newB = b <|+| g
      if (!newMap.contains(newB)) {
        newMap += (newB -> (apply(b) |+| ip))
        toCheck += newB
      }
    }
    if (toCheck.isEmpty) return this
    while (!toCheck.isEmpty) {
      val newAdded = MutableBitSet.empty
      for (ip@InversePair(g, gInv) <- allGenerators; b <- toCheck) {
        val newB = b <|+| g
        if (!newMap.contains(newB)) {
          newMap += (newB -> (newMap.apply(b) |+| ip))
          newAdded += newB
        }
      }
      toCheck = newAdded
    }
    new TransversalExplicit(beta, newMap)
  }
  def updated(newGenerators: Iterable[P], allGenerators: Iterable[P]): TransversalExplicit[P] = {
    if (newGenerators.isEmpty) return this
    updatedPair(newGenerators.map(g => InversePair(g, g.inverse)), allGenerators.map(g => InversePair(g, g.inverse)))
  }
}

object TransversalExplicit extends TransversalBuilder {
  def empty[P](beta: Int)(implicit algebra: Permutation[P]): Transversal[P] =
    new TransversalExplicit(beta, IntMap(beta -> InversePair(algebra.id, algebra.id)))(algebra)
}
