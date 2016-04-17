package net.alasc.named

import net.alasc.algebra._
import net.alasc.finite.{Grp, GrpBuilder}

object Dihedral {

  def shift[G:PermutationBuilder](n: Int): G = Cyclic.shift[G](n)

  def reflection[G:PermutationBuilder](n: Int) =
    PermutationBuilder[G].fromImageFun(n, i => (n - 1) - i)

  def apply[G:PermutationBuilder:GrpBuilder](degree: Int): Grp[G] =
    if (degree < 2) Grp.trivial[G] else
      Grp.fromGeneratorsAndOrder(
        Seq(shift[G](degree), reflection[G](degree)),
        2 * degree)

}
