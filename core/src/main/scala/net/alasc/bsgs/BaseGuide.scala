package net.alasc.bsgs

import scala.annotation.tailrec
import scala.collection.mutable

import net.alasc.algebra.PermutationAction

/** Guide used during the base change. */
trait BaseGuide {

  def iterator: BaseGuideIterator

  def isSatisfiedBy(chain: Chain[_, _]): Boolean = {
    val it = iterator
    @tailrec def check(current: Chain[_, _]): Boolean = current match {
      case node: Node[_, _] =>
        if (!it.checksNext(node.beta, node.isFixed(_)))
          false
        else
          check(node.next)
      case _: Term[_, _] => true
    }
    check(chain)
  }

  /** Returns an incomplete base for the given generators,
    * used when base has to be computed from scratch. */
  def baseAnsatz[G, A <: PermutationAction[G] with Singleton](generators: Iterable[G])(implicit action: A): Seq[Int] = {
    val base = mutable.ArrayBuffer.empty[Int]
    val it = iterator
    @tailrec def rec(remaining: Iterable[G]): Unit =
      if (remaining.nonEmpty && it.hasNext) {
        val sup = remaining.map(action.movedPoints(_)).reduce(_ ++ _)
        if (sup.nonEmpty) {
          val beta = it.next(sup.min, sup, (k: Int) => remaining.forall(g => (action.actr(k, g)) == k))
          base += beta
          rec(remaining.filter(g => action.actr(beta, g) == beta))
        }
      }
    base.result
  }


  /** Returns a full base without online optimizations. Used when no real base change 
    * is possible, only recomputation from scratch. */
  def fullBase: Seq[Int]

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

object BaseGuide {

  object Empty extends BaseGuide {

    def iterator = new BaseGuideIterator {

      def hasNext = false

      def next(beta: Int, easyPoints: collection.Set[Int], isFixed: Int => Boolean) = beta

      override def checksNext(beta: Int, isFixed: Int => Boolean) = true

    }

    def fullBase = Seq.empty

  }

}
