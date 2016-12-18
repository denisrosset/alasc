package net.alasc.named

import spire.math.SafeLong
import net.alasc.algebra._
import net.alasc.finite.{Grp, GrpGroup}
import net.alasc.perms.Perm

object Alternating {

  def order(degree: Int): SafeLong = Symmetric.order(degree) / 2

  def shift(n: Int): Perm =
    if (n % 2 == 1) Cyclic.shift(n) else
      Perm.fromImageFun(n, i => if (i == 0) 0 else (i  % (n - 1)) + 1)

  /** Generates the alternating group on `degree` elements.
    * 
    * For n > 2, the generators taken are
    * - (0 1 2), (0 1 2 ... n-1) for n odd and
    * - (0 1 2), (1 2 ... n-1) for n even.
    */
  def apply(degree: Int)(implicit gb: GrpGroup[Perm]): Grp[Perm] =
    if (degree < 3) Grp.trivial[Perm] else
      Grp.fromGeneratorsAndOrder(
        IndexedSeq(shift(degree), Perm.fromImages(Seq(1,2,0))),
        order(degree)
      )

}
