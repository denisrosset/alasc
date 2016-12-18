package net.alasc.finite

import spire.algebra.{Eq, Group}
import spire.math.SafeLong
import spire.syntax.group._

/** Methods for groups composed of elements of type `G`, which only depend on the group standard operations. */
trait GrpGroup[G] {

  // With no Grp[G] argument

  def trivial: Grp[G]

  def fromGenerators(generators: IndexedSeq[G]): Grp[G]

  def fromGeneratorsAndOrder(generators: IndexedSeq[G], order: SafeLong): Grp[G]

  // with a single Grp[G] argument

  /** Returns the group H = h.inverse grp h. */
  def conjugatedBy(grp: Grp[G], h: G): Grp[G]

  def union(x: Grp[G], y: Grp[G]): Grp[G]

  def intersect(x: Grp[G], y: Grp[G]): Grp[G]

  def smallGeneratingSet(grp: Grp[G]): IndexedSeq[G]

  /** Left cosets. */
  def leftCosetsBy(grp: Grp[G], subgrp: Grp[G]): LeftCosets[G, subgrp.type]

  /** Right cosets. */
  def rightCosetsBy(grp: Grp[G], subgrp: Grp[G]): RightCosets[G, subgrp.type]

}

object GrpGroup {

  @inline final def apply[G](implicit ev: GrpGroup[G]): GrpGroup[G] = ev

  def defaultConjugatedBy[G:Group:GrpGroup](grp: Grp[G], h: G): Grp[G] = {
    val hInv = h.inverse
    GrpGroup[G].fromGeneratorsAndOrder(grp.generators.map(g => hInv |+| g |+| h), grp.order)
  }

}

class GrpGroupSyntax[G](val lhs: Grp[G]) extends AnyVal {

  /** Returns the group H = hInv G h, where G is this group. */
  def conjugatedBy(h: G)(implicit ev: GrpGroup[G]): Grp[G] = ev.conjugatedBy(lhs, h)

  /** Union (closure) of groups. */
  def union(rhs: Grp[G])(implicit ev: GrpGroup[G]): Grp[G] = ev.union(lhs, rhs)

  /** Intersection of groups. */
  def intersect(rhs: Grp[G])(implicit ev: GrpGroup[G]): Grp[G] = ev.intersect(lhs, rhs)

  /** Left cosets. */
  def leftCosetsBy(subgrp: Grp[G])(implicit ev: GrpGroup[G]): LeftCosets[G, subgrp.type] = ev.leftCosetsBy(lhs, subgrp)

  /** Right cosets. */
  def rightCosetsBy(subgrp: Grp[G])(implicit ev: GrpGroup[G]): RightCosets[G, subgrp.type] = ev.rightCosetsBy(lhs, subgrp)

  /** Simplifies the description current group.*/
  def smallGeneratingSet(implicit ev: GrpGroup[G]): IndexedSeq[G] = ev.smallGeneratingSet(lhs)

}
