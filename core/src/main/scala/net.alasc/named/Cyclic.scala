package net.alasc.named

import net.alasc.algebra._
import net.alasc.prep._

object Cyclic {

  def shift[G:PermutationBuilder](n: Int): G =
    PermutationBuilder[G].fromImageFun(n, i => (i + 1) % n)

  def apply[G:PermutationBuilder:PGrpBuilder](degree: Int): PGrp[G] =
    if (degree < 2) PGrpBuilder[G].trivial else
      PGrpBuilder[G].fromGeneratorsAndOrder(Seq(shift[G](degree)), degree)

}
