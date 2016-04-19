package net.alasc.std

import net.alasc.algebra._
import net.alasc.util._

final class UnitFaithfulPermutationAction extends FaithfulPermutationAction[Unit] {
  def actl(g: Unit, p: Int): Int = p
  override def actr(p: Int, g: Unit): Int = p
  def movedPoints(g: Unit): Set[Int] = Set.empty[Int]
  def largestMovedPoint(g: Unit) = NNNone
  def smallestMovedPoint(g: Unit) = NNNone
  def movedPointsUpperBound: Int = -1
  def nMovedPoints(g: Unit) = 0
}

trait UnitInstances {

  implicit final val unitFaithfulPermutationAction = new UnitFaithfulPermutationAction

}
