package net.alasc

trait GroupCoset[F <: FiniteElement[F]] {
  groupSelf: Group[F] =>

  trait BSGSCoset {
    self: BSGSChain =>

    /** Finds the minimal lexicographic representative of the right coset S f.
      * 
      * Given a subgroup S provided by this BSGSChain of the group G and an element f in G, 
      * finds the element s in S such that f' = s f is lexicographically minimal according to
      * domOrdering, and returns s f.
      * 
      * Lexicographic order is a total order with the rule:
      * 
      * d < e <=> (b1**d' << b1**e') or (b1**d' == b1**e' and b2**d' << b2**e') or ...
      * where d' = phi(d), e' = phi(e) and phi is the permutation action of the group,
      * where the order << of domain elements is given by domOrdering and
      * the indices used in the lexicographic ordering are given by the base of
      * the subgroup. The subgroup should have a complete base (b1, b2 ...) = (1, 2, 3 ...)
      * except if you know what you are doing.
      */
    def rightCosetMinimalRepresentativeUsingBSGSBase(f: F)(implicit domOrdering: Ordering[Dom]): F =
      this match {
        // end of BSGS chain, we have our result
        case terminal: BSGSTerminal => f
        case node: BSGSNode if node.transversal.size == 1 =>
          tail.rightCosetMinimalRepresentativeUsingBSGSBase(f)
        case node: BSGSNode => {
          // beta**(sk sk-1 ... s2 s1 f) = (beta**sk) ** (sk-1 ... s2 s1 f)  = b ** partial
          val minimumB = transversal.keysIterator.minBy(b => action(f, b))
          // special case: if minimumB == beta, then u = identity
          val newF = if(minimumB == beta) f else transversal(minimumB).u * f
          tail.rightCosetMinimalRepresentativeUsingBSGSBase(newF)
        }
      }
  }

  trait SubgroupCoset {
    self: Subgroup =>

    /** Finds the minimal lexicographic representative of the right coset S f.
      * 
      * Given this subgroup S of the group G and an element f in G, finds the element
      * s in S such that f' = s f is lexicographically minimal according to domOrdering,
      * and returns s f.
      * 
      * Lexicographic order on permutations is a total order with the rule:
      * 
      * d < e <=> (1**d' < 1**e') or (1**d' == 1**e' and 2**d' < 2**e') or ...
      * where d' = phi(d), e' = phi(e) and phi is the permutation action of the group. 
      */
    def rightCosetMinimalRepresentative(f: F): F =
      subBSGS.withLexicographicBase.rightCosetMinimalRepresentativeUsingBSGSBase(f)(Dom.IntOrder.DomOrdering)
  }
}
