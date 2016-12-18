package net.alasc.bsgs

import scala.reflect.ClassTag

import spire.algebra.{Eq, Group}
import spire.util.Opt

import net.alasc.algebra.PermutationAction
import net.alasc.finite.GrpGroup

trait GrpChainGroup[G] extends GrpGroup[G] {

  implicit def baseChange: BaseChange
  implicit def baseSwap: BaseSwap
  implicit def equ: Eq[G]
  implicit def classTag: ClassTag[G]
  implicit def group: Group[G]
  implicit def schreierSims: SchreierSims

}
