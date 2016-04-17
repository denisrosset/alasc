package net.alasc.named

import net.alasc.algebra._
import net.alasc.finite.{Grp, GrpBuilder}

object Alternating {

  def order(degree: Int): BigInt = Symmetric.order(degree) / 2

  def shift[G:PermutationBuilder](n: Int): G =
    if (n % 2 == 1) Cyclic.shift[G](n) else
      PermutationBuilder[G].fromImageFun(n, i => if (i == 0) 0 else (i  % (n - 1)) + 1)

  /** Generates the alternating group on `degree` elements.
    * 
    * For n > 2, the generators taken are
    * - (0 1 2), (0 1 2 ... n-1) for n odd and
    * - (0 1 2), (1 2 ... n-1) for n even.
    */
  def apply[G:PermutationBuilder:GrpBuilder](degree: Int): Grp[G] =
    if (degree < 3) Grp.trivial[G] else
      Grp.fromGeneratorsAndOrder(
        Seq(shift[G](degree), PermutationBuilder[G].fromImages(Seq(1,2,0))),
        order(degree)
      )

}
