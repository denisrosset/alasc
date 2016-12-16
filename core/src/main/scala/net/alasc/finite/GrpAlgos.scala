package net.alasc.finite

import spire.algebra.{Eq, Group}
import spire.math.SafeLong
import spire.syntax.group._

/** Algorithms for groups composed of elements of type `G`. */
trait GrpAlgos[G] {

  type GG <: Grp[G]

  /** Group operations on type `G`. */
  implicit def group: Group[G]

  /** Equality for type `G`. */
  implicit def equ: Eq[G]

  // With no Grp[G] argument

  def trivial: GG

  def fromGenerators(generators: IndexedSeq[G]): GG

  def fromGeneratorsAndOrder(generators: IndexedSeq[G], order: SafeLong): GG

  def fromGrp(grp: Grp[G]): GG

  // with a single Grp[G] argument

  /** Returns the group H = h.inverse grp h. */
  def conjugatedBy(grp: Grp[G], h: G): GG = {
    val hInv = h.inverse
    fromGeneratorsAndOrder(grp.generators.map(g => hInv |+| g |+| h), grp.order)
  }

  def union(x: Grp[G], y: Grp[G]): GG

  def intersect(x: Grp[G], y: Grp[G]): GG

  def smallGeneratingSet(grp: Grp[G]): IndexedSeq[G] = grp.generators // TODO: real implementation

  /** Left cosets. */
  def leftCosetsBy(grp: Grp[G], subgrp: Grp[G]): LeftCosets[G, subgrp.type]

  /** Right cosets. */
  def rightCosetsBy(grp: Grp[G], subgrp: Grp[G]): RightCosets[G, subgrp.type]

}

trait GrpAlgosImpl[G, GG <: Grp[G]] {


}
