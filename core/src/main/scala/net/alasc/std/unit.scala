package net.alasc.std

import net.alasc.algebra._
import net.alasc.util._

final class UnitPermutationAction extends PermutationAction[Unit] {
  def isFaithful = true
  def actl(g: Unit, p: Int): Int = p
  override def actr(p: Int, g: Unit): Int = p
  override def movedPoints(g: Unit): Set[Int] = Set.empty[Int]
  override def largestMovedPoint(g: Unit) = NNNone
  override def smallestMovedPoint(g: Unit) = NNNone
  def movedPointsUpperBound(g: Unit) = NNNone
  def findMovedPoint(g: Unit) = NNNone
  override def movesAnyPoint(g: Unit) = false
  override def nMovedPoints(g: Unit) = 0
}

trait UnitInstances {

  implicit final val unitPermutationAction = new UnitPermutationAction

}
