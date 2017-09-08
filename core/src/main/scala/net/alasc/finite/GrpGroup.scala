package net.alasc.finite

import spire.algebra.{Eq, Group}
import spire.math.SafeLong
import spire.syntax.group._
import spire.util.Opt

/** Methods for groups composed of elements of type `G`, which only depend on the group standard operations.
  *
  * See [[GrpGroupSyntax]] for documentation of the methods.
  */
trait GrpGroup[G] {

  protected implicit def equ: Eq[G]

  protected implicit def group: Group[G]

  // With no Grp[G] argument

  def trivial: Grp[G]

  def fromGenerators(generators: Seq[G]): Grp[G]

  def fromGeneratorsAndOrder(generators: Seq[G], order: SafeLong): Grp[G]

  // with a single Grp[G] argument

  def conjugatedBy(grp: Grp[G], h: G): Grp[G]  = {
    val hInv = h.inverse
    fromGeneratorsAndOrder(grp.generators.map(g => hInv |+| g |+| h), grp.order)
  }

  /** Returns whether g1 and g2 are part of the same conjugacy class of G. */
  def areConjugate(grp: Grp[G], g1: G, g2: G): Boolean

  /** Returns, if it exists, g such that g1 g = g g2. */
  def findConjugation(grp: Grp[G], g1: G, g2: G): Opt[G]

  /** Returns, if it exists, g such that g1 g = g g2.
    *
    * @param g2CentralizerSubgroup Subgroup of the centralizer of g2 in grp
    */
  def findConjugation(grp: Grp[G], g1: G, g2: G, g2CentralizerSubgroup: Opt[Grp[G]]): Opt[G]

  /** Returns the centralizer of g, a subgroup of grp that satisfies H = {h | h g = g h}. */
  def centralizer(grp: Grp[G], g: G): Grp[G]

  // with two Grp[G] arguments

  def union(x: Grp[G], y: Grp[G]): Grp[G]

  def intersect(x: Grp[G], y: Grp[G]): Grp[G]

  def leftCosetsBy(grp: Grp[G], subgrp: Grp[G]): LeftCosets[G, subgrp.type]

  def rightCosetsBy(grp: Grp[G], subgrp: Grp[G]): RightCosets[G, subgrp.type]

}


object GrpGroup {

  @inline final def apply[G](implicit ev: GrpGroup[G]): GrpGroup[G] = ev

}

class GrpGroupSyntax[G](val lhs: Grp[G]) extends AnyVal {

  // with a single Grp[G] argument

  /** Returns the group H = hInv G h, where G is this group. */
  def conjugatedBy(h: G)(implicit ev: GrpGroup[G]): Grp[G] = ev.conjugatedBy(lhs, h)

  def areConjugate(g1: G, g2: G)(implicit ev: GrpGroup[G]): Boolean = ev.areConjugate(lhs, g1, g2)

  /** Returns, if it exists, g such that g1 g = g g2. */
  def findConjugation(g1: G, g2: G)(implicit ev: GrpGroup[G]): Opt[G] =
    ev.findConjugation(lhs, g1, g2)

  /** Returns, if it exists, g such that g1 g = g g2.
    *
    * @param g2CentralizerSubgroup Subgroup of the centralizer of g2 in grp
    */
  def findConjugation(g1: G, g2: G, g2CentralizerSubgroup: Opt[Grp[G]])(implicit ev: GrpGroup[G]): Opt[G] =
    ev.findConjugation(lhs, g1, g2, g2CentralizerSubgroup)

  /** Returns the centralizer of g, a subgroup of grp that satisfies H = {h | h g = g h}. */
  def centralizer(g: G)(implicit ev: GrpGroup[G]): Grp[G] = ev.centralizer(lhs, g)

  // with two Grp[G] arguments

  /** Union (closure) of groups. */
  def union(rhs: Grp[G])(implicit ev: GrpGroup[G]): Grp[G] = ev.union(lhs, rhs)

  /** Intersection of groups. */
  def intersect(rhs: Grp[G])(implicit ev: GrpGroup[G]): Grp[G] = ev.intersect(lhs, rhs)

  /** Left cosets. */
  def leftCosetsBy(subgrp: Grp[G])(implicit ev: GrpGroup[G]): LeftCosets[G, subgrp.type] = ev.leftCosetsBy(lhs, subgrp)

  /** Right cosets. */
  def rightCosetsBy(subgrp: Grp[G])(implicit ev: GrpGroup[G]): RightCosets[G, subgrp.type] = ev.rightCosetsBy(lhs, subgrp)

}
