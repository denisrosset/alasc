package net.alasc

import scala.util.Random
import scala.reflect.ClassTag
import scala.annotation.tailrec

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
class RandomBag[E <: FiniteElement[E]] private[alasc](private var x0: E, private var x: Array[E], val gen: Random) {
  def random(implicit gen: Random) = randomElement(gen)
  def randomElement(newGen: Random) = {
    require_(gen eq newGen)
    val r = x.length
    val s = gen.nextInt(r)
    @tailrec def genNotS: Int = {
      val num = gen.nextInt(r)
      if (num != s) num else genNotS
    }
    val t = genNotS
    if (gen.nextBoolean) {
      if (gen.nextBoolean) { // e = 1
        x(s) = x(s)*x(t)
        x0 = x0*x(s)
      } else { // e = -1
        x(s) = x(s)*(x(t).inverse)
        x0 = x0*x(s)
      }
    } else {
      if (gen.nextBoolean) { // e = 1
        x(s) = x(t)*x(s)
        x0 = x(s)*x0
      } else { // e = -1
        x(s) = x(t).inverse*x(s)
        x0 = x(s)*x0
      }
    }
    x0
  }
}

object RandomBag {
  def apply[E <: FiniteElement[E]: ClassTag](xseq: Seq[E], id: E, r: Int, n: Int = 50, gen: Random = Random): RandomBag[E] = {
    val k = xseq.length
    require_(r >= k)
    require_(r >= 10)
    var x = new Array[E](r)
    xseq.copyToArray(x)
    for (i <- k until r)
      x(i) = x(i - k)
    val bag = new RandomBag(id, x, gen)
    for (i <- 0 until n) bag.randomElement(gen)
    bag
  }
}
