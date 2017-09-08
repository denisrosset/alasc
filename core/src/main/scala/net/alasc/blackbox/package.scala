package net.alasc

import scala.reflect.ClassTag

import spire.NoImplicit
import spire.algebra.{Eq, Group}

import net.alasc.finite._

package object blackbox {

  protected def discard[T](t: T): Unit = { }

  implicit def BBGrpGroup[G:ClassTag:Eq:Group](implicit no: NoImplicit[GrpGroup[G]]): GrpGroup[G] = {
    discard(no)
    new BBGrpGroup[G]
  }

  implicit def BBGrpPermutationAction[G:ClassTag:Eq:Group](implicit no: NoImplicit[GrpPermutationAction[G]]): GrpPermutationAction[G] = {
    discard(no)
    new BBGrpPermutationAction[G]
  }

  implicit def BBGrpStructure[G:ClassTag:Eq:Group](implicit no: NoImplicit[GrpStructure[G]]): GrpStructure[G] = {
    discard(no)
    new BBGrpStructure[G]
  }

}
