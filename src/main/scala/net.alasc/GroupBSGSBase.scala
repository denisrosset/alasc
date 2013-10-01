package net.alasc

import scala.annotation.tailrec

trait GroupBSGSBase[F <: FiniteElement[F]] {
  groupSelf: Group[F] =>

  trait BSGSBase {
    self: BSGSChain =>

    /** Deterministic base swap.
      * 
      * @return BSGS group with first two base elements swapped.
      * 
      * Based on Derek Holt "Handbook of Computational Group Theory", 2005, page 103.
      * Note that their line 3 is wrong, betaT has to be used instead of betaT1.
      */
    def deterministicBaseSwap: BSGSChain = {
      require_(isInstanceOf[BSGSNode])
      val node = asInstanceOf[BSGSNode]
      require_(!tail.isTerminal)

      var tList = tail.strongGeneratingSet.filter( t => action(t, tail.beta) == tail.beta )
      val beta1 = tail.beta
      var gammaSet = transversal.keysIterator.filter( k => k != beta && k != beta1 ).toSet
      var betaT = makeOrbit(beta, tList)
      var betaT1 = makeOrbit(beta1, tList)
      var beta1Gi = makeOrbit(beta1).updated(strongGeneratingSet, strongGeneratingSet)
      val siz = (transversal.size*tail.transversal.size)/beta1Gi.size
      def exploreGamma {
        val gamma = gammaSet.head
        val (x, xinv) = (transversal(gamma).u, transversal(gamma).uinv)
        if (!tail.transversal.isDefinedAt(action(xinv, beta1))) {
          var o = makeOrbit(gamma, tList)
          gammaSet = gammaSet diff o.orbit
        } else {
          val y = tail.transversal(action(xinv, beta1)).u
          val yx = y*x
          if(!betaT.contains(action(yx, beta))) {
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
      val nS = strongGeneratingSet ++ tList
      val nTrv = makeTransversal(beta1, nS)
      val nS1 = nS.filter( s => action(s, beta1) == beta1 )
      val nTrv1 = makeTransversal(beta, nS1)
      new BSGSNode(nTrv, tList, new BSGSNode(nTrv1, tList.filter( t => action(t, beta1) == beta1), tail.tail))
    }

    def randomizedBaseSwap(implicit r: scala.util.Random): BSGSChain = {
      require_(isInstanceOf[BSGSNode])
      val node = asInstanceOf[BSGSNode]
      require_(!tail.isTerminal)
      val nBeta = tail.beta
      val nBeta1 = beta
      var nTrv = makeTransversal(nBeta)
      var nTrv1 = makeTransversal(nBeta1)
      var nS = strongGeneratingSet
      var nS1 = strongGeneratingSet.filter(g => action(g, nBeta) == nBeta)
      nTrv = nTrv.updated(nS, nS)
      nTrv1 = nTrv1.updated(nS1, nS1)
      val siz = (transversal.size*tail.transversal.size)/nTrv.size
      while (nTrv1.size < siz) {
        val g = random(r)
        val h = g * nTrv(action(g, nBeta)).uinv
        if (!nTrv1.isDefinedAt(action(h, nBeta1))) {
          nTrv1 = nTrv1.updated(List(h), h :: nS)
          nS1 = h :: nS1
          nS = h :: nS
        }
      }
      new BSGSNode(nTrv, nS, new BSGSNode(nTrv1, nS1, tail.tail))
    }
  }
}
