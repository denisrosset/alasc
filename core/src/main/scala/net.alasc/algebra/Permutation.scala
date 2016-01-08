package net.alasc.algebra

import scala.{ specialized => spec }
import scala.annotation.tailrec

import scala.util.Random

import spire.algebra._
import spire.syntax.action._

import net.alasc.util._

/** Type class for Permutation-like objects.
  * 
  * Combines Eq, Group, Signed and Action[Int, _], along with
  * additional methods.
  * 
  * The standard action for the Action[Int, P] is the right action.
  */
trait Permutation[P] extends Group[P] with FaithfulPermutationAction[P] {

  def actl(p: P, k: Int) = actr(k, inverse(p))

  def swapping(i: Int, j: Int): P = {
    def swapFun: (Int => Int) = (k => if (k == i) j else if (k == j) i else k)
    fromSupportAndImageFun(Set(i, j), swapFun)
  }

  def fromImages(images: Seq[Int]): P

  def fromSupportAndImageFun(support: Set[Int], image: Int => Int): P

  def sorting[T: Order](seq: Seq[T]): P = {
    import spire.compat._
    fromImages(seq.zipWithIndex.sortBy(_._1).map(_._2))
  }

  def from[Q](q: Q)(implicit evQ: FaithfulPermutationAction[Q]): P =
    fromSupportAndImageFun(evQ.support(q), k => evQ.actr(k, q))

  def compatibleWith(p: P) = true

  def random(size: Int)(implicit gen: scala.util.Random): P = {
    import spire.std.int._
    sorting(Seq.tabulate(size)(k => gen.nextInt))
  }
  
}

object Permutation {

  def apply[P: Permutation] = implicitly[Permutation[P]]

}
