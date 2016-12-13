package net.alasc.gap3

import net.alasc.finite.{Grp, Rep}
import net.alasc.perms.{FaithfulPermRep, FaithfulPermRepBuilder, Perm}
import spire.math.SafeLong
import net.alasc.perms.default._
//import Rep.algebra._
/*
object Examples {

  val g1 = Perm(0, 1, 2)
  val g2 = Perm(0, 1)
  val generators = Seq(g1, g2)

  def permRepDim(d: Int): FaithfulPermRep[Perm, SafeLong] =
    implicitly[FaithfulPermRepBuilder[Perm]].build[SafeLong](Seq(Perm(0, d)))

  val rep = permRepDim(3)
  val grpInRep = Grp(generators.map(Rep.Of(_, rep)): _*)

  val conj = ARep.conjugation(grpInRep)

}
*/