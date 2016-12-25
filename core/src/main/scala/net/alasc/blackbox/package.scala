package net.alasc

import spire.NoImplicit
import spire.algebra.{Eq, Group}

import net.alasc.finite._

package object blackbox {

  implicit def BBGrpGroup[G:Eq:Group](implicit no: NoImplicit[GrpGroup[G]]): GrpGroup[G] = new BBGrpAlgos[G]

  implicit def BBGrpPermutationAction[G:Eq:Group](implicit no: NoImplicit[GrpPermutationAction[G]]): GrpPermutationAction[G] = new BBGrpAlgos[G]

}
