package net.alasc

import scala.util.Random
import scala.annotation.tailrec
import scala.collection.mutable.ArraySeq

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
class RandomBag[F <: Finite[F]] private[alasc](private var x0: F, private var x: ArraySeq[F], val gen: Random) {
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
  def apply[F <: Finite[F]](xseq: Seq[F], id: F, r: Int, n: Int = 50, gen: Random = Random): RandomBag[F] = {
    val k = xseq.length
    require_(r >= k)
    require_(r >= 10)
    val x = (if (k == 0)
      ArraySeq.fill(r)(id)
    else
      ArraySeq.tabulate(r)(i => xseq(i % k))
    )
    val bag = new RandomBag(id, x, gen)
    for (i <- 0 until n) bag.randomElement(gen)
    bag
  }
}
