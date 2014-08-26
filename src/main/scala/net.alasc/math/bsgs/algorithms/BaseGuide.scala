package net.alasc.math
package bsgs
package algorithms

import net.alasc.algebra.{PermutationAction, Subgroup}

/** Advisor for base changes. The base change algorithm is authorized to ignore the advice. */
trait BaseGuide {
  /** Determines the next base point.
    * 
    * @param beta       Current base point. If the guide no longer has advice, the function returns `beta`. 
    * @param easyPoints Set of points that are easier for the base change; must always contain `beta`.
    * @param isFixed    A function that tests whether a point is fixed by the current stabilizer group in the chain.
    * 
    * @return The next base point, taken from `easyPoints` whenever possible.
    */
  def basePoint(beta: Int, easyPoints: collection.Set[Int], isFixed: Int => Boolean): Int

  /** Moves to the next base point, telling this advisor what point was actually used in the base change.
    * 
    * @param chosenPoint The base point actually used in the base change.
    */
  def moveToNext[P](chosenPoint: Int): Unit
  /** Checks whether the base guide can still give advice, or if the remaining base can be left as it is. */
  def hasAdvice: Boolean
}
