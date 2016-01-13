package net.alasc.prep

import scala.reflect.ClassTag
import scala.util.Random

import spire.algebra.{Eq, Group}
import spire.util.Opt

import net.alasc.algebra.Permutation
import net.alasc.domains.Partition
import net.alasc.finite._
import net.alasc.prep.chain.PGrpChainBuilder

import bsgs._

abstract class PGrpBuilder[G] extends GrpBuilder[G] {

  val defaultRepBuilder: PRepBuilder[G]

  def trivialIn(pRep: FaithfulPRep[G]): PGrp.In[pRep.type, G]

  def fromGeneratorsIn(pRep: FaithfulPRep[G])
    (generators: Iterable[G]): PGrp.In[pRep.type, G]

  def fromGeneratorsAndOrderIn(pRep: FaithfulPRep[G])
    (generators: Iterable[G], order: BigInt): PGrp.In[pRep.type, G]

  def fromGeneratorsRandomElementsAndOrderIn(pRep: FaithfulPRep[G])
    (generators: Iterable[G], randomElement: Random => G, order: BigInt): PGrp.In[pRep.type, G]

  def fromGrpIn(pRep: FaithfulPRep[G])
    (grp: Grp[G]): PGrp.In[pRep.type, G]

  // more precise return types

  def trivial: PGrp[G]

  def fromGenerators(generators: Iterable[G]): PGrp[G]

  def fromGeneratorsAndOrder(generators: Iterable[G], order: BigInt): PGrp[G]

  def fromGeneratorsRandomElementsAndOrder(generators: Iterable[G], randomElement: Random => G, order: BigInt): PGrp[G]

  def fromGrp(grp: Grp[G]): PGrp[G]

}
