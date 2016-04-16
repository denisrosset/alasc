package net.alasc.finite

import spire.util.Opt

case class GrpDef[G](generators: Iterable[G], orderOpt: Opt[BigInt] = Opt.empty[BigInt])

object GrpDef {

  implicit def toGrp[G](grpDef: GrpDef[G])(implicit builder: GrpBuilder[G]): Grp[G] =
    grpDef.orderOpt match {
      case Opt(order) => builder.fromGeneratorsAndOrder(grpDef.generators, order)
      case _ => builder.fromGenerators(grpDef.generators)
    }

}
