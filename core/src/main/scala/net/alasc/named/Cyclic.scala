package net.alasc.named

import net.alasc.algebra._
import net.alasc.finite.{Grp, GrpGroup}
import net.alasc.perms.Perm

object Cyclic {

  def shift(n: Int): Perm =
    Perm.fromImageFun(n, i => (i + 1) % n)

  def apply(degree: Int)(implicit gb: GrpGroup[Perm]): Grp[Perm] =
    if (degree < 2) Grp.trivial[Perm] else
      Grp.fromGeneratorsAndOrder(IndexedSeq(shift(degree)), degree)

}
