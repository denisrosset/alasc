package net.alasc.enum

import scala.collection.generic.CanBuildFrom
import scala.collection.{Set, SetLike}
import scala.reflect.ClassTag

import spire.algebra.Group
import spire.algebra.partial.RightPartialAction

import net.alasc.finite.Grp
import net.alasc.perms.PermutationRepBuilder
import net.alasc.prep._

/** Describes a sequence `T` that can be permuted by a group element `G`. */
trait Permutable[T, G] {

  /** Action of the group on the sequence. */ 
  implicit def action: RightPartialAction[T, G]

  /** Group representation for a sequence instance. */
  def pRep(t: T): FaithfulPRep[G]

  /** Symmetry subgroup of `grp` for the sequence `t`. */
  def symmetryGroup(t: T, grp: Grp[G])(implicit builder: PGrpBuilder[G], T: Enumerable[T]): Grp[G] = {
    val r = pRep(t)
    grp.in(r).fixingPartition(T.partition(t))
  }

}

object Permutable {

  implicit def seq[A, G:PermutationRepBuilder]: Permutable[Seq[A], G] = new PermutableSeq[A, G]

  implicit def array[A: ClassTag, G:PermutationRepBuilder]: Permutable[Array[A], G] = new PermutableArray[A, G]

  implicit def setInt[S <: SetLike[Int, S] with Set[Int], G:Group](pRep: FaithfulPRep[G])(implicit cbf: CanBuildFrom[Nothing, Int, S]): Permutable[S, G] = new PermutableSetInt[S, G](pRep)

}

final class PermutableSetInt[S <: SetLike[Int, S] with Set[Int], G:Group](val pRep: FaithfulPRep[G])(implicit cbf: CanBuildFrom[Nothing, Int, S]) extends Permutable[S, G] {

  def action = imply(pRep.permutationAction) {
    RightPartialAction.fromRightAction(net.alasc.std.set.SetIntPermutationAction[S, G])
  }

  def pRep(t: S): FaithfulPRep[G] = pRep

}

final class PermutableSeq[A, G](implicit val builder: PermutationRepBuilder[G]) extends Permutable[Seq[A], G] {

  def action = imply(builder.permutation) { net.alasc.std.seq.SeqPermutationAction[Seq, A, G] }

  def pRep(t: Seq[A]) = builder.forSize(t.size)

}

final class PermutableArray[A:ClassTag, G](implicit val builder: PermutationRepBuilder[G]) extends Permutable[Array[A], G] {

  def action = imply(builder.permutation) { net.alasc.std.array.ArrayPermutationAction[A, G] }

  def pRep(t: Array[A]) = builder.forSize(t.length)

}
