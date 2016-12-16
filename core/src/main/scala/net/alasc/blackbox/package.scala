package net.alasc

import spire.NoImplicit
import spire.algebra.{Eq, Group}

import net.alasc.finite._

package object blackbox {

  implicit def BBGrpBuilder[G:Eq:Group](implicit no: NoImplicit[GrpAlgos[G]]): GrpAlgos[G] = new BBGrpAlgos[G]

}
