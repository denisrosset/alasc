package net.alasc
package math
package enum

import scala.collection.mutable

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
trait Representatives[T, G] /* extends coll.Iterable[Representative[T, G]] */ {
  /** To be define in an early initializer. */ 
  def t: T
  def grp: Grp[G]

  /** Length of the sequence `t`. */
  def tLength: Int

  /** Retrieves the integer representation of element `t(idx)`. */
// TODO  def tInt(idx: Int): Int

  /** Permutation action of `G` on sequence-like `T`. */
  implicit def actionTG: GroupAction[T, G]

  /** Returns the representation of G specific to `t`. */
  def representation: Representation[G]

  /** Returns the partition given by equivalent elements in `t`. */
  def partition: Partition

  /** Returns the subgroup of `grp` that fixes the `partition` given by `t`. */
  lazy val symGrp: Grp[G] = grp.fixingPartitionW(partition, representation)
}

object Representatives {
  def iterable[A, P](givenT: Seq[A], givenGrp: Grp[P])(implicit givenPR: PermutationRepresentations[P], givenP: Permutation[P]) =
    new {
      val t = givenT
      val grp = givenGrp
      implicit val sequenceTA: Sequence[Seq[A], A] = net.alasc.std.seq.SeqSequence[Seq, A]
      implicit val actionTG: GroupAction[Seq[A], P] = net.alasc.std.seq.SeqPermutationAction[Seq, A, P]
      val representation = givenPR.forSize(t.size)
    } with RepresentativesIterable[Seq[A], P] with ImplIntArrayHashed[Seq[A], A]
}
/*
trait RepresentativesSearchable[T, G] extends RepresentativesIterable[T, G] {
  abstract class SearchableBase(t: T, grp: Grp[G]) extends IterableOf(t, grp) {
    val tIntArray: Array[Int]
    def asIntArray(r: T): Array[Int]
    import grp.{algorithms, algebra, action}
    override def stringPrefix = "Searchable"
    lazy val chainRepr = grp.chain(RefSome(repr))
    lazy val chainReprBasePoints = algorithms.basePointGroups(chainRepr, repr.size)
    def find(r: T): Option[Representative[T, G]] = {
      val rIntArray = asIntArray(r)
      val bo = algorithms.baseOrder(chainRepr.base)(action)
      def rec(level: Int, g: G, chainGrp: Chain[G], chainSym: Grp[G]): Option[Representative[T, G]] = chainGrp match {
        case node: Node[G] =>
          implicit def action = repr.action
          val orbitIt = node.orbit.iterator
          while (orbitIt.hasNext) {
            val beta = node.beta
            val b = orbitIt.next
            val bg = action.actr(b, g)
            if (rIntArray(beta) == tIntArray(bg)) {
              val nextG = node.u(b) |+| g
              var j = 1
              var disagree = false
              val m = chainReprBasePoints(level).length
              while (j < m && !disagree) {
                val c = chainReprBasePoints(level)(j)
                if (rIntArray(c) != tIntArray(c <|+| nextG))
                  disagree = true
                j += 1
              }
              if (!disagree) {
                val (nextSym, transversal) = chainSym.stabilizerW(bg, repr)
                if (transversal.orbit.min(Order.ordering(bo)) == bg) {
                  val res = rec(level + 1, nextG, node.next, nextSym)
                  if (res.nonEmpty)
                    return res
                }
              }
            }
          }
          None
        case _: Term[G] => Some(new Representative[T, G] {
          val element = g.inverse
          def get = t <|+| element
        })
      }
      rec(0, algebra.id, chainRepr, symGrp)
    }
  }
}

trait RepresentativesLexFirst[T, G] extends RepresentativesSearchable[T, G] {

}

object Representatives {
  def iterable[A: Eq] = new RepresentativesIterable[Seq[A], Perm] {
    implicit val actionTG: GroupAction[Seq[A], Perm] = net.alasc.std.seq.SeqPermutationAction[Seq, A, Perm]
    def partition(t: Seq[A]) = Partition.fromSeqEq(t)
    def representation(t: Seq[A]) = Perm.Representations.forSize(t.size)
    def of(t: Seq[A], grp: Grp[Perm]): coll.Iterable[Representative[Seq[A], Perm]] = new IterableOf(t, grp)
  }
  def searchable[A: Eq] = new RepresentativesSearchable[Seq[A], Perm] {
    implicit val actionTG: GroupAction[Seq[A], Perm] = net.alasc.std.seq.SeqPermutationAction[Seq, A, Perm]
    def partition(t: Seq[A]) = Partition.fromSeqEq(t)
    def representation(t: Seq[A]) = Perm.Representations.forSize(t.size)
    class SearchableOf(t: Seq[A], grp: Grp[Perm]) extends SearchableBase(t, grp) {
      val aMap = mutable.HashMap.empty[Any, mutable.BitSet]
      t.indices.foreach { i => aMap.getOrElseUpdate(t(i), mutable.BitSet.empty) += i }
      val tIntArray = Array.tabulate(t.size)( i => aMap(t(i)).head )
      def asIntArray(r: Seq[A]) = {
        require(r.size == t.size)
        Array.tabulate(r.size)( i => aMap.get(r(i)).fold(-1)(_.head) )
      }
    }
    def of(t: Seq[A], grp: Grp[Perm]) = new SearchableOf(t, grp)
  }
}
 */
