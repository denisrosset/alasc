package net.alasc.perms.wrap

import scala.util.Random

import spire.algebra.{Eq, Group}
import spire.math.SafeLong

import net.alasc.finite.Grp
import net.alasc.perms.FaithfulPermRep

class WrapGrp[G, R <: FaithfulPermRep[G] with Singleton](val rep: R,
                                                         val underlying: Grp[R#Wrap])
                                                        (implicit val equ: Eq[G],
                                                         val group: Group[G]) extends Grp[G] {
  /** Iterator through all the group elements. */
  override def iterator: Iterator[G] = underlying.iterator.map(_.underlying)

  override def generators: Iterable[G] = underlying.generators.map(_.underlying)

  override def order: SafeLong = underlying.order

  override def randomElement(random: Random): G = underlying.randomElement(random).underlying

  override def contains(g: G): Boolean =
    if (rep.represents(g)) underlying.contains(rep.Wrap(g)) else false

}
