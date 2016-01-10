package net.alasc.enum

import scala.annotation.tailrec

import spire.algebra.Order
import spire.algebra.partial.RightPartialAction
import spire.syntax.partialAction._
import spire.syntax.cfor._
import spire.util._

import net.alasc.algebra._
import net.alasc.domains._

/** An explicit implementation of `EnumerableSearchable`, where the type of the underlying
  * element `A` is known, and a sequence can be viewed as a map from an element instance to
  * the set of indices where this element is present (see `Groups`).
  */
trait EnumerableSequence[T, A] extends EnumerableSearchable[T] {

  /** Map of element instances to indices where this element appears. */
  type Groups = PartialFunction[A, Set[Int]] with Iterable[(A, Set[Int])] // = nearly a Map[A, Set[Int]]

  def groups(t: T): Groups

  /** Retrieves the element present at the index `idx` in the sequence `t`. */
  def element(t: T, idx: Int): A

  def partition(t: T): Partition =
    Partition(groups(t).iterator.map(_._2).toSeq: _*)

  def commonPartitions(from: T, to: T): Opt[PartitionMap[Set[Int]]] = {
    val fromGroups = groups(from)
    val toGroups = groups(to)
    val maps = fromGroups.toSeq map {
      case (fromK, fromSet) =>
        if (toGroups.isDefinedAt(fromK)) {
          val toSet = toGroups(fromK)
          if (fromSet.size == toSet.size)
            (fromSet -> toSet)
          else
            return Opt.empty[PartitionMap[Set[Int]]]
        } else 
          return Opt.empty[PartitionMap[Set[Int]]]
    }
    Opt(PartitionMap(maps: _*))
  }

}

final class EnumerableSequenceSeqWithHashCode[A] extends EnumerableSequence[Seq[A], A] {

  def groups(t: Seq[A]): Map[A, Set[Int]] = t.zipWithIndex.groupBy(_._1).mapValues(_.map(_._2).toSet)

  def element(t: Seq[A], idx: Int): A = t(idx)

  override def size(t: Seq[A]) = t.size

}

final class EnumerableSequenceArrayWithHashCode[A] extends EnumerableSequence[Array[A], A] {

  def groups(t: Array[A]): Map[A, Set[Int]] = t.zipWithIndex.groupBy(_._1).mapValues(_.map(_._2).toSet)

  def element(t: Array[A], idx: Int): A = t(idx)

  override def size(t: Array[A]) = t.length

}
