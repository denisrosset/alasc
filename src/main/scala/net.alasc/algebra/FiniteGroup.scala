package net.alasc.algebra

import spire.algebra._
import spire.syntax.eq._

/** Type class for finite group elements objects.
  * 
  * Combines Eq and Group.
  */
trait FiniteGroup[F] extends Group[F] with Eq[F] {
  /** Order of `f`, i.e. the number `k` such that
    * `f |+| f ... k times ... |+| f === id`
    */
  def order(f: F): Int = {
    @annotation.tailrec def rec(k: Int, acc: F): Int =
      if (isId(acc)) k else rec(k + 1, op(f, acc))
    rec(1, f)
  }
  // TODO: add optimized conjugatedBy method
}
