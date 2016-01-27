package net.alasc.named

import net.alasc.algebra._
import net.alasc.perms._
import net.alasc.prep._
import net.alasc.syntax.all._

object Dihedral {

  import NamedGroups.generate

  def shift[G:Permutation](n: Int): G = Cyclic.shift[G](n)

  def reflection[G:Permutation](n: Int) =
    Permutation[G].fromImageFun(n, i => (n - 1) - i)

  def apply[G:Permutation:PGrpBuilder](degree: Int): PGrp[G] =
    if (degree < 2) PGrpBuilder[G].trivial else
      PGrpBuilder[G].fromGeneratorsAndOrder(
        Seq(shift[G](degree), reflection[G](degree)),
        2 * degree)

}
