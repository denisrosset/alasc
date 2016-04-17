package net.alasc.named

import net.alasc.algebra._
import net.alasc.finite.{Grp, GrpBuilder}
import net.alasc.perms._
import net.alasc.syntax.all._

object NamedGroups {

  def generate[G:PermutationBuilder:GrpBuilder](generators: Iterable[Perm], order: BigInt): Grp[G] = {
    val generatorsG = generators.map(_.toPermutation[G])
    Grp.fromGeneratorsAndOrder(generatorsG, order)
  }

}
