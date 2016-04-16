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

  /** Returns the subgroup for which `predicate` is satisfied; the test `backtrackTest` is used to
    * prune the search tree.
    *
    * @param backtrackTest Tests if a pair (preimage, image) is valid for an element of the subgroup. False
    *                      positives are allowed, but a false negative would incorrectly prune the tree.
    * @param predicate Tests if an element is member of the subgroup
    * @return the subgroup satisfying `predicate`
    */
  def subgroupFor(grp: PGrp[G], backtrackTest: (Int, Int) => Boolean, predicate: G => Boolean): Grp[G]

}

object PGrpBuilder {

  def apply[G](implicit ev: PGrpBuilder[G]): PGrpBuilder[G] = ev

}
