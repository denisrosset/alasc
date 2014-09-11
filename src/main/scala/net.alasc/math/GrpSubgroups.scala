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
}

class GrpSubgroups[G](val grp: Grp[G]) { // TODO: qualify all calls to grp.chain
  import grp.{representation, algebra, representations, algorithms}
  def fixingPartitionW(partition: Domain#Partition, rp: Representation[G]): Grp[G] =
    Grp.fromChain(algorithms.fixingPartition(grp.chain, partition)(rp.action).toChain, RefSome(rp))
  def fixingPartition(partition: Domain#Partition)(implicit prp: PermutationRepresentations[G]): Grp[G] =
    fixingPartitionW(partition, prp.forSize(partition.size))
  def stabilizerW(b: Int, rp: Representation[G]): (Grp[G], Transversal[G]) = {
    val newChain = algorithms.withBase(grp.chain(RefSome(rp), Seq(b)), Seq(b))(rp.action)
    val (nextChain, transversal) = newChain.detach(b)
    (Grp.fromChain(nextChain, RefSome(rp)), transversal)
  }
  def stabilizer(b: Int)(implicit prp: PermutationRepresentations[G]): (Grp[G], Transversal[G]) = {
    val rp = if (b < representation.size) representation else prp.forSize(b + 1)
    stabilizerW(b, rp)
  }
  def pointwiseStabilizerW(set: Set[Int], rp: Representation[G]): Grp[G] = {
    val mutableChain = algorithms.pointwiseStabilizer(grp.chain, set)(rp.action)
    Grp.fromChain(mutableChain.toChain, RefSome(rp))
  }
  def pointwiseStabilizer(points: Int*)(implicit prp: PermutationRepresentations[G]): Grp[G] = {
    if (points.size == 0) return grp
    val set = Set(points:_*)
    val maxSet = set.max
    val rp = if (maxSet < representation.size) representation else prp.forSize(maxSet + 1)
    pointwiseStabilizerW(set, rp)
  }
  def setwiseStabilizer(set: Set[Int], rp: Representation[G]): Grp[G] = {
    val mutableChain = algorithms.setwiseStabilizer(grp.chain, set)(rp.action)
    Grp.fromChain(mutableChain.toChain, RefSome(rp))
  }
  def setwiseStabilizer(points: Int*)(implicit prp: PermutationRepresentations[G]): Grp[G] = {
    if (points.size == 0) return grp
    val set = Set(points:_*)
    val maxSet = set.max
    val rp = if (maxSet < representation.size) representation else prp.forSize(maxSet + 1)
    setwiseStabilizer(set, rp)
  }
}
