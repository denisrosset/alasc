package net.alasc

import scala.reflect.ClassTag

import spire.NoImplicit
import spire.algebra.{Eq, Group}

import net.alasc.finite._

package object blackbox {

  implicit def BBGrpGroup[G:ClassTag:Eq:Group](implicit no: NoImplicit[GrpGroup[G]]): GrpGroup[G] =
    new BBGrpGroup[G]

  implicit def BBGrpPermutationAction[G:ClassTag:Eq:Group](implicit no: NoImplicit[GrpPermutationAction[G]]): GrpPermutationAction[G] =
    new BBGrpPermutationAction[G]

  implicit def BBGrpStructure[G:ClassTag:Eq:Group](implicit no: NoImplicit[GrpStructure[G]]): GrpStructure[G] =
    new BBGrpStructure[G]

}
