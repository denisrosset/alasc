package net.alasc.math
package bsgs
package algorithms

import net.alasc.algebra.{PermutationAction, Subgroup}

trait BaseGuide {
  def basePoint(easyPoints: collection.Set[Int]): Int
  def moveToNext[P](chosenPoint: Int, nextGenerators: Iterable[P])(implicit action: PermutationAction[P]): Unit
}
