package net.alasc
package bsgs

trait BSGSBase[E <: PermElement[E]] {
  self: BSGSGroup[E] =>

  def cleanedBase: BSGSGroup[E] = this match {
    case g: BSGSGroupTerminal[E] => g
    case g: BSGSGroupNode[E] => {
      if (g.trv.size == 1)
        g.tail.cleanedBase
      else
        new BSGSGroupNode(g.trv, g.sg, g.id, g.tail.cleanedBase)
    }
  }

  def conjugatedBy(e: E): BSGSGroup[E] = this match {
    case g: BSGSGroupTerminal[E] => g
    case g: BSGSGroupNode[E] =>  {
      val einv = e.inverse
      new BSGSGroupNode(g.trv.conjugatedBy(e), g.sg.map(x => einv*x*e), g.id, g.isImmutable, g.tl.conjugatedBy(e))
    }
  }

  /** Deterministic base swap.
    * 
    * @return BSGS group with first two base elements swapped.
    * 
    * Based on Derek Holt "Handbook of Computational Group Theory", 2005, page 103.
    * Note that their line 3 is wrong, betaT has to be used instead of betaT1.
    */
  def deterministicBaseSwap: BSGSGroup[E] = this match {
    case g: BSGSGroupTerminal[E] => throw new IllegalArgumentException("Cannot swap base of BSGS chain terminal")
    case g: BSGSGroupNode[E] => {
      if (g.tail.isTerminal) throw new IllegalArgumentException("Cannot swap base of last element in the BSGS chain.")
      val builder = g.transversal.builder
      var tList = g.tail.strongGeneratingSet.filter( t => t.image(tail.beta) == tail.beta )
      val beta1 = g.tail.beta
      var gammaSet = g.transversal.keysIterator.filter( k => k != beta && k != beta1 ).toSet
      var betaT = OrbitSet.empty(g.beta)
      var betaT1 = OrbitSet.empty(beta1)
      betaT = betaT.updated(tList, tList)
      betaT1 = betaT1.updated(tList, tList)
      var beta1Gi = OrbitSet.empty(beta1)
      beta1Gi = beta1Gi.updated(g.sg, g.sg)
      val siz = (transversal.size*tail.transversal.size)/beta1Gi.size
      def exploreGamma {
        val gamma = gammaSet.head
        val (x, xinv) = g.trv(gamma)
        if (!tail.transversal.isDefinedAt(beta1**xinv)) {
          var o = OrbitSet.empty(gamma)
          o = o.updated(tList, tList)
          gammaSet = gammaSet diff o.orbit
        } else {
          val y = tail.transversal.u(beta1**xinv)
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
      val nS = g.sg ++ tList
      val nTrv = builder.empty(beta1, g.id).updated(nS, nS)
      val nS1 = nS.filter( s => s.image(beta1) == beta1 )
      val nTrv1 = builder.empty(beta, g.id).updated(nS1, nS1)
      new BSGSGroupNode(nTrv, tList, g.id, new BSGSGroupNode(nTrv1, tList.filter( t => t.image(beta1) == beta1), g.id, tail.tail))
    }
  }
  def randomizedBaseSwap(implicit r: scala.util.Random): BSGSGroup[E] = this match {
    case g: BSGSGroupTerminal[E] => throw new IllegalArgumentException("Cannot swap base of BSGS chain terminal")
    case g: BSGSGroupNode[E] => {
      if (tail.isTerminal) throw new IllegalArgumentException("Cannot swap base of last element in the BSGS chain.")
      val nBeta = tail.beta
      val nBeta1 = beta
      val builder = transversal.builder
      var nTrv = builder.empty(nBeta, g.id)
      var nTrv1 = builder.empty(nBeta1, g.id)
      var nS = g.sg
      var nS1 = g.sg.filter(_.image(nBeta) == nBeta)
      nTrv = nTrv.updated(nS, nS)
      nTrv1 = nTrv1.updated(nS1, nS1)
      val siz = (transversal.size*tail.transversal.size)/nTrv.size
      while (nTrv1.size < siz) {
        val g = random(r)
        val h = g.represents * nTrv.uinv(g.image(nBeta))
        if (!nTrv1.isDefinedAt(h.image(nBeta1))) {
          nTrv1 = nTrv1.updated(List(h), h :: nS)
          nS1 = h :: nS1
          nS = h :: nS
        }
      }
      new BSGSGroupNode(nTrv, nS, g.id, new BSGSGroupNode(nTrv1, nS1, g.id, tail.tail))
    }
  }
}

