package net.alasc.math

import scala.util.Random
import scala.annotation.tailrec
import scala.collection.mutable.ArraySeq
import spire.algebra.Group
import spire.syntax.group._

/** Generator of random elements from generators of a group.
  * 
  * A random bag is a set of random group elements that always generates
  * the group;random elements are provided by multiplying elements of the
  * bag and returning one element of the product which is removed from the bag.
  * 
  * The random number generator must not be changed during use.
  * 
  * @note Straight-forward implementation of PRINITIALIZE and PRRANDOM of 
  *       section 3.2.2, pp. 70-71 of Holt
  */
class RandomBag[G: Group] private[math](private var x0: G, private var x: ArraySeq[G]) {
  def randomElement(rand: Random) = {
    val r = x.length
    val s = rand.nextInt(r)
    @tailrec def genNotS: Int = {
      val num = rand.nextInt(r)
      if (num != s) num else genNotS
    }
    val t = genNotS
    if (rand.nextBoolean) {
      if (rand.nextBoolean) { // e = 1
        x(s) = x(s) |+| x(t)
        x0 = x0 |+| x(s)
      } else { // e = -1
        x(s) = x(s) |+| x(t).inverse
        x0 = x0 |+| x(s)
      }
    } else {
      if (rand.nextBoolean) { // e = 1
        x(s) = x(t) |+| x(s)
        x0 = x(s) |+| x0
      } else { // e = -1
        x(s) = x(t).inverse |+| x(s)
        x0 = x(s) |+| x0
      }
    }
    x0
  }
}

object RandomBag {
  def apply[G](xiterable: Iterable[G], givenR: Int = -1, n: Int = 50, rand: Random = Random)(implicit algebra: Group[G]): RandomBag[G] = {
    val k = xiterable.size
    val r = if (givenR < k || givenR < 10) k.max(10) else givenR
    val x = if (k == 0)
      ArraySeq.fill(r)(algebra.id)
    else
      Iterator.continually(xiterable).flatMap(identity).take(r).to[ArraySeq]
    val bag = new RandomBag(algebra.id, x)
    for (i <- 0 until n) bag.randomElement(rand)
    bag
  }
}
