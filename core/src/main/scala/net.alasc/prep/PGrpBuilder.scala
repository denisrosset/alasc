package net.alasc.prep

import scala.util.Random

import net.alasc.finite._

abstract class PGrpBuilder[G] extends GrpBuilder[G] {

  type GG <: PGrp[G]

  val defaultRepBuilder: PRepBuilder[G]

  def trivialIn(pRep0: FaithfulPRep[G]): GG { type R = pRep0.type }

  def fromGeneratorsIn(pRep0: FaithfulPRep[G])
    (generators: Iterable[G]): GG { type R = pRep0.type }

  def fromGeneratorsAndOrderIn(pRep0: FaithfulPRep[G])
    (generators: Iterable[G], order: BigInt): GG { type R = pRep0.type }

  def fromGeneratorsRandomElementsAndOrderIn(pRep0: FaithfulPRep[G])
    (generators: Iterable[G], randomElement: Random => G, order: BigInt): GG { type R = pRep0.type }

  def fromGrpIn(pRep0: FaithfulPRep[G])
    (grp: Grp[G]): GG { type R = pRep0.type }

}

object PGrpBuilder {

  def apply[G](implicit ev: PGrpBuilder[G]): PGrpBuilder[G] = ev

}
