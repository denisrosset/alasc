package net.alasc.blackbox

import scala.reflect.ClassTag

import spire.algebra.{Eq, Group}
import spire.math.SafeLong
import net.alasc.finite.{Grp, GrpGroup, GrpStructure}

class BBGrpStructure[G](implicit
                    val classTag: ClassTag[G],
                    val group: Group[G],
                    val grpGroup: GrpGroup[G],
                    val equ: Eq[G]
                   ) extends GrpStructure[G] {

  type GG = BBGrp[G]

  def fromGrp(grp: Grp[G]): GG = grp match {
    case bb: BBGrp[G] => bb
    case _ => new BBGrp(grp.generators, grp.iterator.toSet)
  }

  override def smallGeneratingSet(grp: Grp[G]): Seq[G] = {
    def computeOrder(gens: Seq[G]): SafeLong = SafeLong(Dimino[G](gens).length)
    GrpStructure.deterministicReduceGenerators(grp.generators, grp.order, computeOrder).getOrElseFast(grp.generators)
  }

}
