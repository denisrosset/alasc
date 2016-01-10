package net.alasc

import scala.reflect.ClassTag

import spire.algebra.{Eq, Group}
import spire.util.Opt

import net.alasc.algebra.Permutation
import net.alasc.finite.Grp
import net.alasc.perms.PermutationRepBuilder
import net.alasc.prep.{FaithfulPRep, PGrp, PGrpBuilder}
import net.alasc.prep.bsgs.BaseGuide
import net.alasc.prep.chain.PGrpChainBuilder

final class RichGrp[G](val lhs: Grp[G]) {

  def in(pRep: FaithfulPRep[G])(implicit builder: PGrpBuilder[G]): PGrp.In[pRep.type, G] =
    builder.fromGrpIn(pRep)(lhs)

}

final class RichGrpWithParent[G](val lhs: Grp[G]) {

  def in(pRep: FaithfulPRep[G])(implicit builder: PGrpBuilder[G]): PGrp.In[pRep.type, G] =
    builder.fromGrpIn(pRep)(lhs)

}

package object prep {

  implicit def richGrp[G](lhs: Grp[G]): RichGrp[G] = new RichGrp[G](lhs)

  implicit def permutationGrp[G:ClassTag:Eq:Group:Permutation:PermutationRepBuilder:PGrpBuilder](lhs: Grp[G]): PGrp[G] =
    implicitly[PGrpBuilder[G]].fromGrp(lhs)

  object Deterministic {

    import bsgs._

    implicit def pGrpChainBuilder[G:ClassTag:Eq:Group:PRepBuilder]: PGrpChainBuilder[G] = {
      implicit def schreierSims: SchreierSims = SchreierSims.deterministic
      implicit def baseSwap: BaseSwap = BaseSwap.deterministic
      implicit def baseChange: BaseChange = BaseChange.swap
      new chain.PGrpChainBuilder[G]
    }

  }

  object Randomized {


  }

}
