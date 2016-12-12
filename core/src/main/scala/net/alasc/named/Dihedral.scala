package net.alasc.named

import net.alasc.algebra._
import net.alasc.finite.{Grp, GrpBuilder}
import net.alasc.perms.Perm

object Dihedral {

  def shift(n: Int): Perm = Cyclic.shift(n)

  def reflection(n: Int) =
    PermutationBuilder[Perm].fromImageFun(n, i => (n - 1) - i)

  def apply(degree: Int)(implicit gb: GrpBuilder[Perm]): Grp[Perm] =
    if (degree < 2) Grp.trivial[Perm] else
      Grp.fromGeneratorsAndOrder(
        IndexedSeq(shift(degree), reflection(degree)),
        2 * degree)

}
