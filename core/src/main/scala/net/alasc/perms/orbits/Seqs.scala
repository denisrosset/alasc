package net.alasc.perms.orbits

import scala.annotation.tailrec
import scala.collection.generic.CanBuildFrom
import scala.collection.SeqLike
import scala.reflect.ClassTag

import spire.algebra.Order
import spire.math.{SafeLong, Searching}
import spire.syntax.cfor._
import spire.syntax.group._
import spire.util.Opt

import net.alasc.algebra.PermutationAction
import net.alasc.bsgs.{BaseGuideLex, GrpChain, GrpChainPermutationAction}
import net.alasc.partitions.Partition
import net.alasc.finite.Grp
import net.alasc.perms.{Perm, PermAlgebra}
import net.alasc.util.{NNNone, NNOption}

object Seqs {

  trait Representatives[ST <: SeqLike[T, ST], G, T, A <: PermutationAction[G] with Singleton] {
    implicit val gcpa: GrpChainPermutationAction[G]
    implicit def cbf: CanBuildFrom[Nothing, T, ST]

    import gcpa.{baseChange, baseSwap, group, schreierSims}

    def seq: Seq[T] = Seq.tabulate(n)(i => element(intLabels(i)))
    def grp: GrpChain[G, A]
    def symGrp: GrpChain[G, A]
    def action: A
    protected def intLabels: Array[Int]
    protected def n = intLabels.length
    protected def element(label: Int): T
    protected def labelOf(t: T): NNOption
    def iterator: Iterator[G]
    protected def seqUnder(g: G): ST = {
      val gInverse = g.inverse
      val b = cbf()
      b.sizeHint(n)
      cforRange(0 until n) { i =>
        b += element(intLabels(action.actr(i, gInverse)))
      }
      b.result()
    }

    def seqIterator: Iterator[ST] = iterator.map(seqUnder)

    def size: SafeLong = grp.quotientOrder / symGrp.quotientOrder

    protected def toIntLabels(that: Seq[T]): Opt[Array[Int]] =
      if (that.size != n) Opt.empty[Array[Int]] else {
        val thatArray = new Array[Int](n)
        // fills the index array for "that" sequence, returns false if an element is not part of the original seq.
        @tailrec @inline def recFillArray(it: Iterator[T], i: Int): Boolean =
        if (i == n) true else labelOf(it.next) match {
          case NNOption(elInd) =>
            thatArray(i) = elInd
            recFillArray(it, i + 1)
          case _ => false
        }
        if (recFillArray(that.iterator, 0)) Opt(thatArray) else Opt.empty[Array[Int]]
      }

    def findPermutationTo(that: Seq[T]): Opt[G] = toIntLabels(that) match {
      case Opt(thatArray) => RepresentativesArrayInt.permutationTo[G, A](intLabels, thatArray, grp, symGrp)
      case _ => Opt.empty[G]
    }

  }

  final case class UnorderedRepresentatives[ST <: SeqLike[T, ST], G, T, A <: PermutationAction[G] with Singleton]
    (grp: GrpChain[G, A], symGrp: GrpChain[G, A], action: A, distinctElements: IndexedSeq[T], indexMap: Map[T, Int], intLabels: Array[Int])
    (implicit val cbf: CanBuildFrom[Nothing, T, ST], val gcpa: GrpChainPermutationAction[G]) extends Representatives[ST, G, T, A] {

    def iterator = grp.rightCosetsBy(symGrp).representativesIterator
    def element(label: Int) = distinctElements(label)
    def labelOf(t: T) = indexMap.get(t).fold(NNNone)(NNOption(_))
  }

  final case class OrderedRepresentatives[ST <: SeqLike[T, ST], G, T:ClassTag:Order, A <: PermutationAction[G] with Singleton]
  (grp: GrpChain[G, A], symGrp: GrpChain[G, A], action: A, sortedElements: Array[T], nUnique: Int, intLabels: Array[Int])
  (implicit val cbf: CanBuildFrom[Nothing, T, ST], val gcpa: GrpChainPermutationAction[G]) extends Representatives[ST, G, T, A] {

    import gcpa.{baseChange, baseSwap, group, schreierSims}

    protected lazy val repAI = RepresentativesArrayInt(intLabels, grp, symGrp)

    protected def labelOf(t: T): NNOption = {
      val index = spire.math.Searching.search(sortedElements, t, 0, nUnique - 1)
      if (index >= 0) NNOption(index) else NNNone
    }

    protected def element(label: Int) = sortedElements(label)

    def iterator: Iterator[G] = repAI.orderedIterator.map(_.gInverse.inverse)

    def findIndexOf(that: Seq[T]): Opt[SafeLong] = toIntLabels(that) match {
      case Opt(thatArray) => repAI.find(thatArray) match {
        case Opt(tb) => Opt(tb.index)
        case _ => Opt.empty[SafeLong]
      }
      case _ => Opt.empty[SafeLong]
    }

    def minimum: G = RepresentativesArrayInt.findPermutationToMinimal(intLabels, grp, symGrp)

    def seqMinimum: ST = seqUnder(minimum)

    def maximum: G = RepresentativesArrayInt.findPermutationToMaximal(intLabels, grp, symGrp)

    def seqMaximum: ST = seqUnder(maximum)

    def atIndex(index: SafeLong): G =
      if (index.isZero) minimum
      else if ((size - index).isOne) maximum
      else repAI.apply(index).gInverse.inverse

    def seqAtIndex(index: SafeLong): ST = seqUnder(atIndex(index))

  }

  object Representatives {

    def unordered[CC[T] <: SeqLike[T, CC[T]], T](grp: Grp[Perm], seq: Seq[T])(implicit cbf: CanBuildFrom[Nothing, T, CC[T]], gcpa: GrpChainPermutationAction[Perm]): Representatives[CC[T], Perm, T, PermAlgebra.type] =
      unordered[CC, Perm, T](grp, Perm.algebra, seq)

    def unordered[CC[T] <: SeqLike[T, CC[T]], T](grp: Grp[Perm], seq: Seq[T], symGrpOpt: Opt[Grp[Perm]])(implicit cbf: CanBuildFrom[Nothing, T, CC[T]], gcpa: GrpChainPermutationAction[Perm]): Representatives[CC[T], Perm, T, PermAlgebra.type] =
      unordered[CC, Perm, T](grp, Perm.algebra, seq, symGrpOpt)


    def unordered[CC[T] <: SeqLike[T, CC[T]], G:GrpChainPermutationAction, T](grp: Grp[G], action: PermutationAction[G], seq: Seq[T])(implicit cbf: CanBuildFrom[Nothing, T, CC[T]]): Representatives[CC[T], G, T, action.type] =
      unordered(grp, action, seq, Opt.empty[Grp[G]])

    def unordered[CC[T] <: SeqLike[T, CC[T]], G:GrpChainPermutationAction, T](grp: Grp[G], action: PermutationAction[G], seq: Seq[T], symGrpOpt: Opt[Grp[G]])(implicit cbf: CanBuildFrom[Nothing, T, CC[T]]): Representatives[CC[T], G, T, action.type] = {
      val n = seq.size
      val distinctElements = seq.toIndexedSeq.distinct
      val indexMap = distinctElements.zipWithIndex.toMap
      val partition = Partition.fromSeq(seq)
      val seqElements = Array.tabulate(seq.size)(i => indexMap(seq(i)))
      val grpInAction = GrpChainPermutationAction[G].fromGrp(grp, action, Opt(BaseGuideLex(n)))
      val symGrp = symGrpOpt match {
        case Opt(sg) => GrpChainPermutationAction[G].fromGrp(sg, action)
        case _ => GrpChainPermutationAction[G].fixingPartition(grpInAction, action, partition)
      }
      UnorderedRepresentatives[CC[T], G, T, action.type](grpInAction, symGrp, action, distinctElements, indexMap, seqElements)
    }

    def ordered[CC[T] <: SeqLike[T, CC[T]], T:ClassTag:Order](grp: Grp[Perm], seq: Seq[T])(implicit cbf: CanBuildFrom[Nothing, T, CC[T]], gcpa: GrpChainPermutationAction[Perm]): OrderedRepresentatives[CC[T], Perm, T, PermAlgebra.type] =
      ordered[CC, Perm, T](grp, Perm.algebra, seq)

    def ordered[CC[T] <: SeqLike[T, CC[T]], T:ClassTag:Order](grp: Grp[Perm], seq: Seq[T], symGrpOpt: Opt[Grp[Perm]])(implicit cbf: CanBuildFrom[Nothing, T, CC[T]], gcpa: GrpChainPermutationAction[Perm]): OrderedRepresentatives[CC[T], Perm, T, PermAlgebra.type] =
      ordered[CC, Perm, T](grp, Perm.algebra, seq, symGrpOpt)

    def ordered[CC[T] <: SeqLike[T, CC[T]], G:GrpChainPermutationAction, T:ClassTag:Order](grp: Grp[G], action: PermutationAction[G], seq: Seq[T])(implicit cbf: CanBuildFrom[Nothing, T, CC[T]]): OrderedRepresentatives[CC[T], G, T, action.type] =
      ordered(grp, action, seq, Opt.empty[Grp[G]])

    def ordered[CC[T] <: SeqLike[T, CC[T]], G:GrpChainPermutationAction, T:ClassTag:Order](grp: Grp[G], action: PermutationAction[G], seq: Seq[T], symGrpOpt: Opt[Grp[G]])(implicit cbf: CanBuildFrom[Nothing, T, CC[T]]): OrderedRepresentatives[CC[T], G, T, action.type] = {
      val n = seq.size
      val sortedElements = seq.toArray
      spire.math.Sorting.sort(sortedElements)
      @tailrec def removeDuplicates(ordIndex: Int, testIndex: Int): Int =
        if (testIndex == sortedElements.length) ordIndex + 1 // end of array
        else if (Order[T].compare(sortedElements(ordIndex), sortedElements(testIndex)) == 0) // elements are the same
          removeDuplicates(ordIndex, testIndex + 1)
        else // elements are different, so we start a new possible run
          removeDuplicates(testIndex, testIndex + 1)
      val nUnique = if (sortedElements.length > 0) removeDuplicates(0, 1) else 0
      val seqElements = Array.tabulate(n)(i => Searching.search(sortedElements, seq(i), 0, nUnique - 1))
      val grpInAction = GrpChainPermutationAction[G].fromGrp(grp, action, Opt(BaseGuideLex(n)))
      val partition = Partition.fromSeq(seqElements)
      val symGrp = symGrpOpt match {
        case Opt(sg) => GrpChainPermutationAction[G].fromGrp(sg, action)
        case _ => GrpChainPermutationAction[G].fixingPartition(grpInAction, action, partition)
      }
      OrderedRepresentatives[CC[T], G, T, action.type](grpInAction, symGrp, action, sortedElements, nUnique, seqElements)
    }

  }

}
