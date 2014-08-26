package net.alasc.math
package bsgs
package algorithms

import net.alasc.algebra.{PermutationAction, Subgroup}

/** Advisor for base changes. The base change algorithm is authorized to ignore the advice. */
trait BaseGuide {
  /** Determines the next base point.
    * 
    * @param easyPoints Set of points that are easier for the base change.
    * 
    * @return The next base point, taken from `easyPoints` whenever possible.
    */
  def basePoint(easyPoints: collection.Set[Int]): Int

  /** Moves to the next base point, telling this advisor what point was actually used in the base change.
    * 
    * @param chosenPoint The base point actually used in the base change.
    * @param isFixed     A function that tests whether a point is fixed by the next group in the stabilizer chain.
    */
  def moveToNext[P](chosenPoint: Int, isFixed: Int => Boolean): Unit
}
