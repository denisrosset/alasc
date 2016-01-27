package net.alasc.named

import scala.annotation.tailrec
import scala.util.Random

import net.alasc.algebra._
import net.alasc.perms._
import net.alasc.prep._
import net.alasc.syntax.all._

object Alternating {

  def order(degree: Int): BigInt = Symmetric.order(degree) / 2

  def shift[G:Permutation](n: Int): G =
    if (n % 2 == 1) Cyclic.shift[G](n) else
      Permutation[G].fromImageFun(n, i => if (i == 0) 0 else (i  % (n - 1)) + 1)

  /** Generates the alternating group on `degree` elements.
    * 
    * For n > 2, the generators taken are
    * - (0 1 2), (0 1 2 ... n-1) for n odd and
    * - (0 1 2), (1 2 ... n-1) for n even.
    */
  def apply[G:Permutation:PGrpBuilder](degree: Int): PGrp[G] =
    if (degree < 3) PGrpBuilder[G].trivial else
      PGrpBuilder[G].fromGeneratorsAndOrder(
        Seq(shift[G](degree), Permutation[G].fromImages(Seq(1,2,0))),
        order(degree)
      )

}
