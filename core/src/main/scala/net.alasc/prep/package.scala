package net.alasc


import spire.algebra.Eq

import net.alasc.finite.Grp
import net.alasc.perms.PermutationRepBuilder
import net.alasc.prep.{FaithfulPRep, PGrp, PGrpBuilder}

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

  implicit def permutationGrp[G:Eq:PermutationRepBuilder:PGrpBuilder](lhs: Grp[G]): PGrp[G] =
    implicitly[PGrpBuilder[G]].fromGrp(lhs)

}
