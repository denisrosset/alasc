package com.faacets
package perm
package bsgs

import scala.annotation.tailrec
import scala.util.Random
import language.implicitConversions
import scala.language.higherKinds

case class BSGSGroup[E <: PermElement[E]](
  val trv: TransLike[E],
  private[bsgs] val sgList: List[E],
  private[bsgs] val next: BSGSGroup[E]) extends BSGSLike[E] with PermGroup[BSGSElement[E]] {

  object baseTranspose {
    /*
     def deterministic[T <: Trans[T, E]](trvFactory: TransCompanion[T, E]) = {
     require(next ne null)
     val nBeta = next.trv.beta
     val nBeta1 = trv.beta
     val id = identity.represents
     var T = next.sgList.filter( t => t.image(next.trv.beta) == next.trv.beta )
     var gammaSet = nTrv.keysIterator.filter( k => k != nBeta && k != nBeta1 ).toSet
     val siz = (trv.size*next.trv.size)/nTrv.size
     def test {
     val gamma = gammaSet.head
     if (!nTrv1.isDefinedAt(nBeta1**nTrv.u(gamma))) {
     gammaSet = gammaSet - gamma**
     }
     while (nTrv1.size < siz) {

     val g = random(r)
     val h = g.represents * nTrv.uinv(g.image(nBeta))
     if (!nTrv1.isDefinedAt(h.image(nBeta1))) {
     nTrv1 = nTrv1.addingGenerator(h)
     nS1 = h :: nS1
     nS = h :: nS
     }
     }
     BSGSGroup(nTrv, nS, BSGSGroup(nTrv1, nS1, next.next))
     }*/

    def randomized[T[E <: PermElement[E]] <: Trans[T[E], E]](transComp: TransCompanion[T])(implicit r: scala.util.Random) = {
      require(next ne null)
      val nBeta = next.trv.beta
      val nBeta1 = trv.beta
      val id = identity.represents
      var nTrv = transComp.empty(nBeta, id)
      var nTrv1 = transComp.empty(nBeta1, id)
      var nS = sgList
      var nS1 = sgList.filter(_.image(nBeta) == nBeta)
      nTrv = nTrv.updated(nS, nS)
      nTrv1 = nTrv1.updated(nS1, nS1)
      val siz = (trv.size*next.trv.size)/nTrv.size
      while (nTrv1.size < siz) {
        val g = random(r)
        val h = g.represents * nTrv.uinv(g.image(nBeta))
        if (!nTrv1.isDefinedAt(h.image(nBeta1))) {
          nTrv1 = nTrv1.updated(List(h), h :: nS)
          nS1 = h :: nS1
          nS = h :: nS
        }
      }
      BSGSGroup(nTrv, nS, BSGSGroup(nTrv1, nS1, next.next))
    }
  }

  def mapElements[F <: PermElement[F]](f: E => F): BSGSGroup[F] = new BSGSGroup[F](
    trv.mapValues(f), sgList.map(f(_)), nextNotNullOr(next.mapElements(f), null)
  )
  def size: Int = nextNotNullOr(next.size, 1) + 1
  def nextInChain: Option[BSGSGroup[E]] = nextNotNullOr(Some(next), None)
  def compatible(e: BSGSElement[E]) = (e.g == this) && nextNotNullOr(next.compatible(e.nextEl), true)
  def elements = for {
    b <- trv.keysIterator
    ne <- nextNotNullOr(next.elements, List(null).iterator)
  } yield BSGSElement(b, ne, this)
  def contains(e: BSGSElement[E]) = trv.isDefinedAt(e.b) && nextNotNullOr(next.contains(e.nextEl), true)
  /** Constructs a BSGS element from a sequence of transversal indices. */
  def fromSequence(sequence: List[Dom]): BSGSElement[E] =
    BSGSElement(sequence.head, nextNotNullOr(next.fromSequence(sequence.tail), null), this)

  /** Constructs a BSGS element from a base image. */
  def fromBaseImage(baseImage: List[Dom]): BSGSElement[E] =
    BSGSElement(baseImage.head, nextNotNullOr(next.fromBaseImage(baseImage.tail.map(k => trv.uinv(baseImage.head).image(k))), null), this)

  def generators = sgList.iterator.map(sift(_)._1)

  def trvElement(b: Dom, level: Int = 0): BSGSElement[E] = BSGSElement(if(level == 0) b else trv.beta, nextNotNullOr(next.trvElement(b, level - 1), null), this)

  def identity = BSGSElement(trv.beta, nextNotNullOr(next.identity, null), this)

  def random(implicit gen: scala.util.Random) =
    BSGSElement(trv.random(gen)._1, nextNotNullOr(next.random(gen), null), this)

  def degree = sgList.head.size

  def fromExplicit(p: Perm): Option[BSGSElement[E]] = {
    implicit def conversion(e: E) = e.explicit
    val (sequence, remaining) = basicSift(p)
    if (remaining.isIdentity)
      Some(fromSequence(sequence))
    else
      None
  }

  def sift(e: E): (BSGSElement[E], E) = {
    val (sequence, remaining) = basicSift(e)
    (fromSequence(sequence), remaining)
  }

  def toTeX = TeX("{}^{"+degree+"}_{"+order+"} \\text{BSGS} \\left ( \\begin{array}{" + "c"*size + "}" + base.mkString(" & ") + "\\\\" + transversalSizes.mkString(" & ") + "\\end{array} \\right )")

  def cleanedBase: BSGSGroup[E] = {
    if (trv.size == 1)
      nextNotNullOr(next.cleanedBase, null)
    else
      BSGSGroup[E](trv, sgList, nextNotNullOr(next.cleanedBase, null))
  }
}
