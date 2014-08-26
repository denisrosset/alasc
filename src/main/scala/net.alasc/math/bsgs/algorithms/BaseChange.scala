package net.alasc.math
package bsgs
package algorithms

import scala.annotation.tailrec

import scala.collection.BitSet
import scala.collection.immutable
import scala.collection.mutable

import spire.algebra.Order
import spire.syntax.groupAction._
import spire.syntax.group._

import net.alasc.algebra.{PermutationAction, Subgroup}
import net.alasc.syntax.check._
import net.alasc.util._

trait BaseChange[P] extends Algorithms[P] {
  /** Change the base after the element `after` in `mutableChain` by `newBase`.
    * 
    * @param mutableChain Mutable chain on which to perform the base change.
    * @param after        Element after which the base is replaced.
    * @param newBase      New base to use, will be extended if it is not a complete base.
    */
  def changeBase(mutableChain: MutableChain[P], newBase: Seq[Int])(implicit action: PermutationAction[P]): Unit
}

trait BaseChangeGuided[P] extends BaseChange[P] {
  def changeBase(mutableChain: MutableChain[P], baseGuide: BaseGuide)(implicit action: PermutationAction[P]): Unit
}
