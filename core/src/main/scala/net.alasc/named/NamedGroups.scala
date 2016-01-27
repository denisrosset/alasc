package net.alasc.named

import net.alasc.algebra._
import net.alasc.perms._
import net.alasc.prep._
import net.alasc.syntax.all._

object NamedGroups {

  def generate[G:Permutation:PGrpBuilder](generators: Iterable[Perm], order: BigInt): PGrp[G] = {
    val generatorsG = generators.map(_.to[G])
    PGrpBuilder[G].fromGeneratorsAndOrder(generatorsG, order)
  }

}
