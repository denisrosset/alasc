package net.alasc.named

import net.alasc.algebra._
import net.alasc.perms._
import net.alasc.prep._
import net.alasc.syntax.all._

object Cyclic {

  import NamedGroups.generate

  def shift[G:Permutation](n: Int): G =
    Permutation[G].fromImageFun(n, i => (i + 1) % n)

  def apply[G:Permutation:PGrpBuilder](degree: Int): PGrp[G] =
    if (degree < 2) PGrpBuilder[G].trivial else
      PGrpBuilder[G].fromGeneratorsAndOrder(Seq(shift[G](degree)), degree)

}
