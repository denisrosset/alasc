package net.alasc.perms.orbits

import scala.annotation.tailrec

import spire.algebra.Order
import spire.syntax.cfor._
import spire.util.Opt

import net.alasc.algebra.PermutationAction
import net.alasc.bsgs.{GrpChain, GrpChainPermutationAction}
import net.alasc.domains.Partition
import net.alasc.finite.Grp

object Seqs {

  trait Representatives[G, T, A <: PermutationAction[G] with Singleton] {

    def seq: Seq[T]

    def symGrp: Grp[G]

    def action: A

    def iterator: Iterator[G]

    def findPermutationTo(that: Seq[T]): Opt[G]

  }

  final case class UnorderedRepresentatives[G:GrpChainPermutationAction, T, A <: PermutationAction[G] with Singleton]
    (grp: GrpChain[G, A], symGrp: GrpChain[G, A], action: A, distinctElements: IndexedSeq[T], indexMap: Map[T, Int], seqElements: Array[Int])
                                            extends Representatives[G, T, A] {

    def n = seqElements.length
    def seq: Seq[T] = Seq.tabulate(n)(i => distinctElements(seqElements(i)))

    lazy val partition = Partition.fromSeq(seqElements)
    lazy val symGrp = grp.fixingPartition(action, partition)

    def iterator = grp.rightCosetsBy(symGrp).iterator.map(_.representative)

    def findPermutationTo(that: Seq[T]): Opt[G] = {
      require(that.size == n)
      val thatArray = new Array[Int](n)
      // fills the index array for "that" sequence, returns false if an element is not part of the original seq.
      @tailrec @inline def recFillArray(it: Iterator[T], i: Int): Boolean =
        if (i == n) true else indexMap.get(it.next) {
          case Some(elInd) =>
            thatArray(i) = elInd
            recFillArray(it, i + 1)
          case None => false
        }
      if (!recFillArray(that.iterator, 0)) return Opt.empty[G]
      RepresentativesArrayInt.permutationTo[G, A](seqElements, thatArray, grp, symGrp)
    }

  }

  object Representatives {
/*
    def unordered[G, T](grp: Grp[G], action: PermutationAction[G], seq: Seq[T])(implicit ev: GrpChainPermutationAction[G]): Representatives[G, T, action.type] = {

    }*/


  }

}
