package net.alasc.enum

import spire.algebra.{Group, Order}
import spire.syntax.group._
import spire.syntax.action._
import spire.util._

import net.alasc.finite._
import net.alasc.prep._
import net.alasc.prep.bsgs._
import net.alasc.prep.chain._

abstract class RepresentativesSearchable[T, G] extends Representatives[T, G] {

  implicit def builder: PGrpChainBuilder[G]

  implicit def enumerable: EnumerableSearchable[T]

  def chainInRepresentation = builder.fromGrpIn(pRep, Opt(BaseGuideLex(enumerable.size(t))))(grp).chain

  def chainInRepresentationBasePointGroups = SubgroupSearch.basePointGroups(chainInRepresentation, pRep.size)

  def findRepresentative(some: T): Opt[Representative[T, G]]

}

final class RepresentativesSearchableImpl[T, G](val t: T, val grp: Grp[G])(implicit val builder: PGrpChainBuilder[G], val enumerable: EnumerableSearchable[T], val permutable: Permutable[T, G]) extends RepresentativesSearchable[T, G] {

  override lazy val chainInRepresentation = super.chainInRepresentation

  override lazy val chainInRepresentationBasePointGroups = super.chainInRepresentationBasePointGroups

  override lazy val partition = super.partition

  override lazy val symGrp = super.symGrp

  def findRepresentative(r: T): Opt[Representative[T, G]] =
    enumerable.commonPartitions(t, r).flatMap { cp =>
      import grp.group
      val cpSeq: Seq[(Set[Int], Set[Int])] = cp.blocks
      val n = enumerable.size(t)
      val bo = BaseOrder[G](pRep.permutationAction, chainInRepresentation.base)
      val tIntArray = new Array[Int](n)
      val rIntArray = new Array[Int](n)
      cp.blocks.zipWithIndex.foreach {
        case ((tInd, rInd), i) =>
          tInd.foreach { tIntArray(_) = i }
          rInd.foreach { rIntArray(_) = i }
      }
      def rec(level: Int, g: G, chainGrp: Chain[G], chainSym: Grp[G]): Opt[Representative[T, G]] = chainGrp match {
        case node: Node[G] =>
          implicit def action = pRep.permutationAction
          val orbitIt = node.orbit.iterator
          val beta = node.beta
          while (orbitIt.hasNext) {
            val b = orbitIt.next
            val bg = action.actr(b, g)
            if (rIntArray(beta) == tIntArray(bg)) {
              val nextG = node.u(b) |+| g
              var j = 1
              var disagree = false
              val m = chainInRepresentationBasePointGroups(level).length
              while (j < m && !disagree) {
                val c = chainInRepresentationBasePointGroups(level)(j)
                if (rIntArray(c) != tIntArray(c <|+| nextG))
                  disagree = true
                j += 1
              }
              if (!disagree) {
                val (nextSym, transversal) = chainSym.in(pRep).stabilizerTransversal(bg)
                if (transversal.orbit.min(Order.ordering(bo)) == bg) {
                  val res = rec(level + 1, nextG, node.next, nextSym)
                  if (res.nonEmpty)
                    return res
                }
              }
            }
          }
          Opt.empty[Representative[T, G]]
        case _: Term[G] => Opt(Representative(t, g.inverse))
      }
      rec(0, Group[G].id, chainInRepresentation, symGrp)
    }

}
