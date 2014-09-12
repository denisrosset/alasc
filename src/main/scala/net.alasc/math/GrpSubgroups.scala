package net.alasc
package math

import scala.language.implicitConversions

import scala.annotation.tailrec
import scala.util.Random

import spire.syntax.group._
import spire.syntax.groupAction._

import net.alasc.algebra._
import net.alasc.syntax.all._
import net.alasc.util._
import bsgs._
import algorithms._

trait GrpSubgroupsImplicits {
  implicit def GrpSubgroups[G](grp: Grp[G]): GrpSubgroups[G] = new GrpSubgroups[G](grp)
  implicit def GrpLexElements[G](grp: Grp[G]): GrpLexElements[G] = new GrpLexElements[G](grp)
}

class GrpSubgroups[G](val lhs: Grp[G]) { // TODO: qualify all calls to grp.chain
  import lhs.{representation, algebra, representations, algorithms}
  def union(rhs: Grp[G]): Grp[G] = lhs.lattice.join(lhs, rhs)
  def intersect(rhs: Grp[G]): Grp[G] = lhs.lattice.meet(lhs, rhs)
  def hasSubgroup(rhs: Grp[G]): Boolean = lhs.lattice.gteqv(lhs, rhs)
  def hasProperSubgroup(rhs: Grp[G]): Boolean = lhs.lattice.gt(lhs, rhs)
  def isSubgroupOf(rhs: Grp[G]): Boolean = lhs.lattice.lteqv(lhs, rhs)
  def isProperSubgroupOf(rhs: Grp[G]): Boolean = lhs.lattice.lt(lhs, rhs)

  def &(rhs: Grp[G]) = intersect(rhs)
  def |(rhs: Grp[G]) = union(rhs)

  def /(rhs: Grp[G]): LeftCosets[G] = {
    require(rhs.generators.forall(lhs.contains(_)))
    new LeftCosets(lhs, rhs)
  }
  def \(rhs: Grp[G]): RightCosets[G] = {
    require(lhs.generators.forall(rhs.contains(_)))
    new RightCosets(lhs, rhs)
  }
  def fixingPartitionW(partition: Domain#Partition, rp: Representation[G]): Grp[G] =
    Grp.fromChain(algorithms.fixingPartition(lhs.chain, partition)(rp.action).toChain, RefSome(rp))
  def fixingPartition(partition: Domain#Partition)(implicit prp: PermutationRepresentations[G]): Grp[G] =
    fixingPartitionW(partition, prp.forSize(partition.size))
  def stabilizerW(b: Int, rp: Representation[G]): (Grp[G], Transversal[G]) = {
    val newChain = lhs.chain(rp, BaseGuideSeq(Seq(b)))
    val (nextChain, transversal) = newChain.detach(b)
    (Grp.fromChain(nextChain, RefSome(rp)), transversal)
  }
  def stabilizer(b: Int)(implicit prp: PermutationRepresentations[G]): (Grp[G], Transversal[G]) = {
    val rp = if (b < representation.size) representation else prp.forSize(b + 1)
    stabilizerW(b, rp)
  }
  def pointwiseStabilizerW(set: Set[Int], rp: Representation[G]): Grp[G] = {
    val mutableChain = algorithms.pointwiseStabilizer(lhs.chain, set)(rp.action)
    Grp.fromChain(mutableChain.toChain, RefSome(rp))
  }
  def pointwiseStabilizer(points: Int*)(implicit prp: PermutationRepresentations[G]): Grp[G] = {
    if (points.size == 0) return lhs
    val set = Set(points:_*)
    val maxSet = set.max
    val rp = if (maxSet < representation.size) representation else prp.forSize(maxSet + 1)
    pointwiseStabilizerW(set, rp)
  }
  def setwiseStabilizer(set: Set[Int], rp: Representation[G]): Grp[G] = {
    val mutableChain = algorithms.setwiseStabilizer(lhs.chain, set)(rp.action)
    Grp.fromChain(mutableChain.toChain, RefSome(rp))
  }
  def setwiseStabilizer(points: Int*)(implicit prp: PermutationRepresentations[G]): Grp[G] = {
    if (points.size == 0) return lhs
    val set = Set(points:_*)
    val maxSet = set.max
    val rp = if (maxSet < representation.size) representation else prp.forSize(maxSet + 1)
    setwiseStabilizer(set, rp)
  }
}
