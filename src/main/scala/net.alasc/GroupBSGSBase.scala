package net.alasc

import scala.annotation.tailrec

trait GroupBSGSBase[F <: FiniteElement[F]] {
  groupSelf: Group[F] =>

  trait BSGSBase {
    self: BSGSChain =>

    def conjugatedBy(f: F): BSGSChain = conjugatedBy(f, f.inverse)
    def conjugatedBy(f: F, finv: F): BSGSChain = this match {
      case terminal: BSGSTerminal => terminal
      case node: BSGSNode =>
        new BSGSNode(transversal.conjugatedBy(f, finv), strongGeneratingSet.map(x => finv*x*f), tail.conjugatedBy(f, finv))
    }

    def removingRedundantBasePoints: BSGSChain = this match {
      case terminal: BSGSTerminal => terminal
      case node: BSGSNode if transversal.size == 1 => tail.removingRedundantBasePoints
      case node: BSGSNode => {
        val newTail = tail.removingRedundantBasePoints
        if (newTail eq tail)
          this
        else
          new BSGSNode(transversal, strongGeneratingSet, newTail)
      }
    }

    /** Changes the current base. The base of the returned BSGSChain will start
      * with newBase, but can be longer if needed. No redundant base points will be 
      * kept in these additional points.
      */
    def withBase(newBase: List[Dom]): BSGSChain = options.baseChangeStrategy match {
      case BaseSwapOnly => withBaseNoConjugation(newBase)
      case BaseSwapAndConjugation => withBaseConjugation(newBase)
      case BaseFromScratch => withBaseFromScratch(newBase)
    }

    def withBaseFromScratch(newBase: List[Dom]): BSGSChain = options.useRandomizedAlgorithms match {
      case true =>
        BSGSChain.randomSchreierSims(newBase, randomElement, order)
      case false =>
        BSGSChain.deterministicSchreierSims(newBase, generators)
    }

    def withBaseConjugation(newBase: List[Dom]): BSGSChain = {
      val (newChain, f, finv) = withBaseConjugationHelper(newBase, identity, identity)
      newChain.conjugatedBy(f)
    }

    def withBaseConjugationHelper(newBase: List[Dom], f: F, finv: F): (BSGSChain, F, F) = newBase match {
      case hd :: tl => {
        val alpha = action(finv, hd)
        if (!isTerminal) {
          if (!base.contains(alpha) && transversal.isDefinedAt(alpha)) {
            val (newTail, newF, newFinv) =
              tail.withBaseConjugationHelper(tl, transversal(alpha).u*f, finv*transversal(alpha).uinv)
            val newStrongGeneratingSet = (strongGeneratingSet diff newTail.strongGeneratingSet) ::: newTail.strongGeneratingSet
            val newNode = new BSGSNode(transversal, newStrongGeneratingSet, newTail)
            return ((newNode, newF, newFinv))
          }
        }
        val swappedNode = withHeadBasePoint(alpha)
        val (newTail, newF, newFinv) = swappedNode.tail.withBaseConjugationHelper(tl, f, finv)
        val newStrongGeneratingSet = (swappedNode.strongGeneratingSet diff newTail.strongGeneratingSet) ::: newTail.strongGeneratingSet
        val newNode = new BSGSNode(swappedNode.transversal, newStrongGeneratingSet, newTail)
        ((newNode, newF, newFinv))
      }
      case Nil => (removingRedundantBasePoints, f, finv)
    }

    def withBaseNoConjugation(newBase: List[Dom]): BSGSChain = newBase match {
      case hd :: tl => {
        val newHead = withHeadBasePoint(hd)
        newHead.check
        val newTail = newHead.tail.withBaseNoConjugation(tl)
        newTail.check
        val newStrongGeneratingSet = 
          (newHead.strongGeneratingSet diff newTail.strongGeneratingSet) ::: newTail.strongGeneratingSet
        new BSGSNode(newHead.transversal, newStrongGeneratingSet, newTail)
      }
      case Nil => removingRedundantBasePoints
    }

    /** Called on a BSGSChain containing basePoint, returns
      * a BSGSChain with basePoint at head.
      */
    def putExistingBasePointInHead(basePoint: Dom): BSGSChain = {
      if (beta == basePoint)
        return this
      val newTail = tail.putExistingBasePointInHead(basePoint)
      val newStrongGeneratingSet = (strongGeneratingSet diff newTail.strongGeneratingSet) ::: newTail.strongGeneratingSet
      val beforeSwap = new BSGSNode(transversal, newStrongGeneratingSet,
        newTail)
      beforeSwap.baseSwap
    }

    def withHeadBasePoint(basePoint: Dom): BSGSChain = {
      val withPoint = if (base.contains(basePoint)) this else insertBasePoint(basePoint)
      withPoint.putExistingBasePointInHead(basePoint)
    }

    /** Returns an updated BSGSChain with the new basePoint inserted.
      * 
      * The current BSGSChain must not contains basePoint
      */
    def insertBasePoint(basePoint: Dom): BSGSChain = this match {
      case terminal: BSGSTerminal =>
        new BSGSNode(makeTransversal(basePoint), Nil, terminal)
      case node: BSGSNode => {
        val orbit = makeOrbit(basePoint, strongGeneratingSet)
        if (orbit.size > 1)
          new BSGSNode(transversal, strongGeneratingSet, tail.insertBasePoint(basePoint))
        else
          new BSGSNode(makeTransversal(basePoint), strongGeneratingSet, this)
      }
    }

    def baseSwap: BSGSChain = options.useRandomizedAlgorithms match {
      case true => randomizedBaseSwap(options.randomGenerator)
      case false => deterministicBaseSwap
    }

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
      val nS = (tList diff strongGeneratingSet) ::: strongGeneratingSet
      val nTrv = makeTransversal(beta1, nS)
      val nS1 = nS.filter( s => action(s, beta1) == beta1 )
      val nTrv1 = makeTransversal(beta, nS1)
      new BSGSNode(nTrv, nS, new BSGSNode(nTrv1, nS1, tail.tail))
    }

    def randomizedBaseSwap(r: scala.util.Random): BSGSChain = {
      require_(isInstanceOf[BSGSNode])
      val node = asInstanceOf[BSGSNode]
      require_(!tail.isTerminal)
      val nBeta = tail.beta
      val nBeta1 = beta
      var nS = strongGeneratingSet
      var nS1 = strongGeneratingSet.filter(g => action(g, nBeta) == nBeta)
      var nTrv = makeTransversal(nBeta, nS)
      var nTrv1 = makeTransversal(nBeta1, nS1)
      val siz = (transversal.size*tail.transversal.size)/nTrv.size
      while (nTrv1.size < siz) {
        val g = self.randomElement(r)
        val h = g * nTrv(action(g, nBeta)).uinv
        if (!nTrv1.isDefinedAt(action(h, nBeta1))) {
          nS = h :: nS
          nS1 = h :: nS1
          nTrv1 = nTrv1.updated(List(h), nS1)
          nTrv = nTrv.updated(List(h), nS)
        }
      }
      new BSGSNode(nTrv, nS, new BSGSNode(nTrv1, nS1, tail.tail))
    }
  }
}
