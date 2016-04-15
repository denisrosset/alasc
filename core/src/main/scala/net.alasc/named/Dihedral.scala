package net.alasc.named

import net.alasc.algebra._
import net.alasc.prep._

object Dihedral {

  def shift[G:PermutationBuilder](n: Int): G = Cyclic.shift[G](n)

  def reflection[G:PermutationBuilder](n: Int) =
    PermutationBuilder[G].fromImageFun(n, i => (n - 1) - i)

  def apply[G:PermutationBuilder:PGrpBuilder](degree: Int): PGrp[G] =
    if (degree < 2) PGrpBuilder[G].trivial else
      PGrpBuilder[G].fromGeneratorsAndOrder(
        Seq(shift[G](degree), reflection[G](degree)),
        2 * degree)

}
