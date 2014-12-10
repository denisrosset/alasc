package net.alasc
package math
package enum

import net.alasc.math.guide.BaseGuideLex

import scala.collection.mutable
import scala.reflect.ClassTag

import spire.algebra.{Eq, GroupAction, Order}
import spire.syntax.group._
import spire.syntax.groupAction._

import net.alasc.algebra._
import net.alasc.syntax.sequence._
import net.alasc.util._

import bsgs._

/** Describes the enumeration of representatives of a sequence-like `t` under permutation by `grp`.
  * 
  * `t` can be a sequence, a regular Java array, a linear algebra vector; all that matters is that
  * an integer array representation (for speed) of the elements of `t` can be extracted.
  */
trait Representatives[T, G] {
  /** To be define in an early initializer. */ 
  val t: T
  val grp: Grp[G]
  implicit def finiteGroupG: FiniteGroup[G] = grp.algorithms.algebra

  /** Permutation action of `G` on sequence-like `T`. */
  implicit def actionTG: GroupAction[T, G]

  /** Length of the sequence `t`. */
  def tLength: Int

  /** Retrieves the integer representation of element `t(idx)`, using non-negative integers. */
  def tInt(idx: Int): Int

  /** Returns the maximal integer value used in the representation of `t`. */
  def maxInt: Int

  def seqInt(seq: T, idx: Int): NNOption

  /** Returns the representation of G specific to `t`. */
  def representation: Representation[G]

  /** Returns the partition given by equivalent elements in `t`. */
  lazy val partition: Domain#Partition = Domain(tLength).Partition.fromSeq(Seq.tabulate(tLength)(idx => tInt(idx)))

  /** Returns the subgroup of `grp` that fixes the `partition` given by `t`. */
  lazy val symGrp: Grp[G] = grp.fixingPartition(partition, representation)
}

trait RepresentativesOrdered[T, G] extends Representatives[T, G] {
  /** Retrieves the (ordered) integer representation of element `t(idx)`, using non-negative integers. */
  def tInt(idx: Int): Int

  def chainInRepresentation = grp.chain(representation, BaseGuideLex(tLength))
}

object Representatives {
  def apply[A, P](givenT: Seq[A], givenGrp: Grp[P])(implicit givenPR: PermutationRepresentations[P], givenP: Permutation[P]) =
    new {
      val t = givenT
      val grp = givenGrp
      implicit val sequenceTA: Sequence[Seq[A], A] = net.alasc.std.seq.SeqSequence[Seq, A]
      implicit val actionTG: GroupAction[Seq[A], P] = net.alasc.std.seq.SeqPermutationAction[Seq, A, P].Forced
      val representation = givenPR.forSize(t.size)
    } with RepresentativesIterableUnordered[Seq[A], P] with RepresentativesSearchable[Seq[A], P] with SequencesHash[Seq[A], A, P] {
      lazy val chainInRepresentation = grp.chain(representation)
    }
  def apply[A, P](givenT: Seq[A], givenGrp: Grp[P], givenRep: Representation[P]) =
    new {
      val t = givenT
      val grp = givenGrp
      implicit val finiteGroup = givenGrp.algebra
      implicit val permAction: FaithfulPermutationAction[P] = givenRep.action
      implicit val sequenceTA: Sequence[Seq[A], A] = net.alasc.std.seq.SeqSequence[Seq, A]
      implicit val actionTG: GroupAction[Seq[A], P] = net.alasc.std.seq.SeqPermutationAction[Seq, A, P].Forced
      val representation = givenRep
    } with RepresentativesIterableUnordered[Seq[A], P] with RepresentativesSearchable[Seq[A], P] with SequencesHash[Seq[A], A, P] {
      lazy val chainInRepresentation = grp.chain(representation)
    }
}

object RepresentativesOrdered {
  def apply[A, P](givenT: Seq[A], givenGrp: Grp[P])(implicit givenPR: PermutationRepresentations[P], givenP: Permutation[P], givenOrderA: Order[A]) =
    new {
      val t = givenT
      val grp = givenGrp
      implicit val finiteGroup = givenGrp.algebra
      implicit val classTagG = givenGrp.gClassTag
      implicit val orderA: Order[A] = givenOrderA
      implicit val sequenceTA: Sequence[Seq[A], A] = net.alasc.std.seq.SeqSequence[Seq, A]
      implicit val actionTG: GroupAction[Seq[A], P] = net.alasc.std.seq.SeqPermutationAction[Seq, A, P].Forced
      val representation = givenPR.forSize(t.size)
    } with SequencesOrdered[Seq[A], A, P] with RepresentativesSeq[Seq[A], P] with RepresentativesHead[Seq[A], P]
  def apply[A, P](givenT: Seq[A], givenGrp: Grp[P], givenRep: Representation[P])(implicit givenOrderA: Order[A]) =
    new {
      val t = givenT
      val grp = givenGrp
      implicit val finiteGroup = givenGrp.algebra
      implicit val classTagG = givenGrp.gClassTag
      implicit val orderA: Order[A] = givenOrderA
      implicit val permAction: FaithfulPermutationAction[P] = givenRep.action
      implicit val sequenceTA: Sequence[Seq[A], A] = net.alasc.std.seq.SeqSequence[Seq, A]
      implicit val actionTG: GroupAction[Seq[A], P] = net.alasc.std.seq.SeqPermutationAction[Seq, A, P].Forced
      val representation = givenRep
    } with SequencesOrdered[Seq[A], A, P] with RepresentativesSeq[Seq[A], P] with RepresentativesHead[Seq[A], P]
}
