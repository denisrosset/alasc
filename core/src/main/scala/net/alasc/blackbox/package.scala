package net.alasc

import spire.NoImplicit
import spire.algebra.{Eq, Group}

import net.alasc.finite._

package object blackbox {

  implicit def BBGrpBuilder[G:Eq:Group](implicit no: NoImplicit[GrpGroup[G]]): GrpGroup[G] = new BBGrpAlgos[G]

}
