package net.alasc.named

import net.alasc.algebra._
import net.alasc.finite.{Grp, GrpBuilder}

object Cyclic {

  def shift[G:PermutationBuilder](n: Int): G =
    PermutationBuilder[G].fromImageFun(n, i => (i + 1) % n)

  def apply[G:PermutationBuilder:GrpBuilder](degree: Int): Grp[G] =
    if (degree < 2) Grp.trivial[G] else
      Grp.fromGeneratorsAndOrder(IndexedSeq(shift[G](degree)), degree)

}
