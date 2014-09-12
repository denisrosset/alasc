package net.alasc.math
package bsgs
package algorithms

import scala.annotation.tailrec
import scala.collection.mutable

import spire.syntax.groupAction._

import net.alasc.algebra.{FaithfulPermutationAction, Subgroup}
import net.alasc.syntax.permutationAction._

trait BaseGuide {
  def iterator: BaseGuideIterator
  def isSatisfiedBy[P](chain: Chain[P]): Boolean = {
    val it = iterator
    @tailrec def check(current: Chain[P]): Boolean = current match {
      case node: Node[P] =>
        if (!it.checksNext(node.beta, node.isFixed(_)))
          false
        else
          check(node.next)
      case _: Term[P] => true
    }
    check(chain)
  }
  /** Returns a full base without online optimizations. Used when no real base change is possible, only recomputation
    * from scratch. */
  def fullBase: Seq[Int]
}

object BaseGuide {
  def empty = new BaseGuide {
    def iterator = new BaseGuideIterator {
      def hasNext = false
      def next(beta: Int, easyPoints: collection.Set[Int], isFixed: Int => Boolean) = beta
      def checksNext(beta: Int, isFixed: Int => Boolean) = true
    }
    def fullBase = Seq.empty
  }
}

/** Iterator to guide base changes. */
trait BaseGuideIterator {
  /** Checks whether the base guide can still give advice, or if the remaining base can be left as it is. */
  def hasNext: Boolean

  /** If the iterator is non-empty, advises the next base point, and advances the iterator. Otherwise,
    * returns `beta`.
    * 
    * @param beta       Current base point. If the guide no longer has advice, the function returns `beta`. 
    * @param easyPoints Set of points that are easier for the base change; must always contain `beta`.
    * @param isFixed    A function that tests whether a point is fixed by the current stabilizer group in the chain.
    * 
    * @return The next base point, taken from `easyPoints` whenever possible when the iterator is not empty,
    *         otherwise `beta`.
    */
  def next(beta: Int, easyPoints: collection.Set[Int], isFixed: Int => Boolean): Int

  /** Checks if the next point in an already constructed chain satisfies the guide.
    * 
    * After it returns false, reuse of the iterator produces undefined results.
    */
  def checksNext(beta: Int, isFixed: Int => Boolean): Boolean =
    next(beta, Set(beta), isFixed) == beta
}
