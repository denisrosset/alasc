package net.alasc.wreath

import spire.math.SafeLong
import spire.util.Opt

import net.alasc.finite.{Grp, GrpGroup}

case class GrpDef[G](generators: Seq[G], orderOpt: Opt[SafeLong] = Opt.empty[SafeLong])

object GrpDef {

  implicit def toGrp[G](grpDef: GrpDef[G])(implicit builder: GrpGroup[G]): Grp[G] =
    grpDef.orderOpt match {
      case Opt(order) => builder.fromGeneratorsAndOrder(grpDef.generators, order)
      case _ => builder.fromGenerators(grpDef.generators)
    }

}
