package net.alasc.finite

import scala.util.Random

import spire.util.Opt

case class GrpDef[G](generators: Iterable[G], orderOpt: Opt[BigInt] = Opt.empty[BigInt], randomElementOpt: Opt[Random => G] = Opt.empty[Random => G])

object GrpDef {

  implicit def toGrp[G](grpDef: GrpDef[G])(implicit builder: GrpBuilder[G]): Grp[G] =
    grpDef.orderOpt match {
      case Opt(order) => grpDef.randomElementOpt match {
        case Opt(randomElement) =>
          builder.fromGeneratorsRandomElementsAndOrder(grpDef.generators, randomElement, order)
        case _ =>
          builder.fromGeneratorsAndOrder(grpDef.generators, order)
      }
      case _ => builder.fromGenerators(grpDef.generators)
    }

}
