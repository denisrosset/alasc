package com.faacets
package perm
package bsgs

import scala.annotation.tailrec
import scala.util.Random
import language.implicitConversions
import scala.language.higherKinds

case class BSGSGroup[E <: PermElement[E]](val trv: TransLike[E],
  private[bsgs] val sgList: List[E],
  private[bsgs] val next: BSGSGroup[E]) extends BSGSLike[E] with PermGroup[BSGSElement[E]] {
  object DomainOrdering extends Ordering[Dom] {
    lazy val domainOrder = {
      val a = Array.fill[Int](degree)(-1)
      val b = base
      for ( (bel, i) <- b.zipWithIndex ) a(bel._0) = i
      var k = base.length
      for ( i <- 0 until degree ) {
        if (a(i) == -1) {
          a(i) = k
          k += 1
        }
      }
      a
    }
    def compare(a: Dom, b: Dom) = Ordering.Int.compare(domainOrder(a._0), domainOrder(b._0))
  }

  object BSGSOrdering extends Ordering[BSGSElement[E]] {
    def compare(a: BSGSElement[E], b: BSGSElement[E]): Int = {
      for (bel <- base) {
        val ord = DomainOrdering.compare(a.image(bel), b.image(bel))
        if (ord != 0)
          return ord
      }
      0
    }
  }

  object ElementOrdering extends Ordering[E] {
    def compare(a: E, b: E): Int = {
      for (bel <- base) {
        val ord = DomainOrdering.compare(a.image(bel), b.image(bel))
        if (ord != 0)
          return ord
      }
      0
    }
  }

  def orderedIterator(uPrev: E): Iterator[E] = for {
    b <- trv.keysIterator.toList.sortBy(uPrev.image).toIterator
    uThis = trv.u(b) * uPrev
    u <- nextNotNullOr(next.orderedIterator(uThis), Iterator(uThis))
  } yield u
  /** From Holt, p. 114 GENERALSEARCH */
  def generalSearch(uPrev: E, level: Int, test: (E, Int) => Boolean): Iterator[E] = for {
    b <- trv.keysIterator.toList.sortBy(uPrev.image).toIterator
    uThis = trv.u(b) * uPrev if test(uThis, level)
    u <- nextNotNullOr(next.generalSearch(uThis, level + 1, test), Iterator(uThis))
  } yield u

  object baseTranspose {
    /** Deterministic base swap.
      * 
      * @param transComp  Transversal companion object.
      * 
      * @return BSGS group with first two base elements swapped.
      * 
      * Based on Derek Holt "Handbook of Computational Group Theory", 2005, page 103.
      * Note that their line 3 is wrong, betaT should be used instead of betaT1.
      */
    def deterministic = {
      require(next ne null)
      val builder = trv.builder
      val id = identity.represents
      var tList = next.sgList.filter( t => t.image(next.trv.beta) == next.trv.beta )
      val beta = trv.beta
      val beta1 = next.trv.beta
      var gammaSet = trv.keysIterator.filter( k => k != beta && k != beta1 ).toSet
      var betaT = OrbitSet.empty(beta)
      var betaT1 = OrbitSet.empty(beta1)
      betaT = betaT.updated(tList, tList)
      betaT1 = betaT1.updated(tList, tList)
      var beta1Gi = OrbitSet.empty(beta1)
      beta1Gi = beta1Gi.updated(sgList, sgList)
      val siz = (trv.size*next.trv.size)/beta1Gi.size
      def exploreGamma {
        val gamma = gammaSet.head
        val (x, xinv) = trv(gamma)
        if (!next.trv.isDefinedAt(beta1**xinv)) {
          var o = OrbitSet.empty(gamma)
          o = o.updated(tList, tList)
          gammaSet = gammaSet diff o.orbit
        } else {
          val y = next.trv.u(beta1**xinv)
          val yx = y*x
          if(!betaT.contains(beta**yx)) {
            tList = yx :: tList
            betaT = betaT.updated(List(yx), tList)
            betaT1 = betaT1.updated(List(yx), tList)
            gammaSet = gammaSet diff betaT.orbit
          }
        }
      }
      while (betaT.size < siz) {
        assert(!gammaSet.isEmpty)
        exploreGamma
      }
      val nS = sgList ++ tList
      val nTrv = builder.empty(beta1, id).updated(nS, nS)
      val nS1 = nS.filter( s => s.image(beta1) == beta1 )
      val nTrv1 = builder.empty(beta, id).updated(nS1, nS1)
      BSGSGroup(nTrv, tList, BSGSGroup(nTrv1, tList.filter( t => t.image(beta1) == beta1), next.next))
    }
    
    def randomized(implicit r: scala.util.Random) = {
      require(next ne null)
      val nBeta = next.trv.beta
      val nBeta1 = trv.beta
      val id = identity.represents
      val builder = trv.builder
      var nTrv = builder.empty(nBeta, id)
      var nTrv1 = builder.empty(nBeta1, id)
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
