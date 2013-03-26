package com.faacets.perm

/** A permutation group with its stabilizer chain described by a base, a strong generating set,
  * and transversals.
  * The underlying permutation type is parametrized by P, and the type of transversals
  * is parametrized by T.
  * One should probably use the Schreier-Sims construction instead of constructing the BSGSGroup
  * object directly.
  */
case class BSGSGroup[P <: Permutation[P], T <: Transversal[P]]
  (identity: P, base: Vector[Domain], strongGeneratingSet: Vector[P], transversals: Vector[T]) extends PermutationGroup[P] {
  val degree = identity.domainSize
  val m = base.length /** Length of the stabilizer chain. */

  override def generatingSet = strongGeneratingSet
  override def order: Int = (1 /: transversals)((p:Int, kv:T) => kv.size*p)
  override def contains(perm: P): Boolean = ???

  /** Checks this BSGS construction for consistency */
  def verify = {
    // Verifies that the strong generating set does not contains the identity
    // TODO: verify that the SGS is really a SGS
    def verifyStrongGeneratingSet = strongGeneratingSet.forall(!_.isIdentity)
    // Checks that the only element that fixes the base pointwise is the identity
    def verifyBase = strongGeneratingSet.forall(g => base.exists(g.hasInSupport(_)))
    // Verifies that the transversals have support in the relevant domain
    def verifyTransversals: Boolean = {
      for (i <- 0 until m) {
        val u = transversals(i)
        for (b <- u.orbitIterator) {
          val g = u(b)
          if (g.image(base(i)) != b)
            return false
          for (j <- 0 until i)
            if (g.image(j) != j)
              return false
        }
      }
      return true
    }
    verifyStrongGeneratingSet && verifyBase && verifyTransversals
  }

  /** Produces a random element */
  def randomElement = {
    var g = identity
    for (u <- transversals)
      g = u.elementsIterator.drop(scala.util.Random.nextInt(u.size)).next() * g
    g
  }

  def iterator = {
    def iter(i: Int): Iterator[P] = {
      if (i == m) return List(identity).iterator
      if (i == m - 1) return transversals(i).elementsIterator
      for(g <- transversals(i).elementsIterator; h <- iter(i+1)) yield g*h
    }
    iter(0)
  }

  def sift(g: P, i: Int): (P, Int) = {
    if (i >= m)
      return (g, i)
    val beta = g.image(base(i))
    if (!transversals(i).contains(beta))
      return (g, i - 1)
    sift(g * transversals(i), i + 1)
  }
}

object BSGSGroup {


}

/*
class BSGSGroup[P <: Permutation[P], T <: Transversal[P]](B: List[Domain], S: List[P], U: List[T]) extends PermutationGroup[P] {
  override def generatingSet = S
  override def verify: Boolean = {
    for (i <- 0 until base.length) {
      val fixed = base.take(i)
      val u = transversals(i).elementsIterator.toList.map(_.images)
      for (g <- u; f <- fixed)
        return false
    }
    // TODO: add more checks
    true
  }
  override def contains(g: P): Boolean = {
    val (siftee, m) = sift(g)
    m == base.size && siftee.isIdentity
  }

  def sift(g: P, j: Int, k: Int): (P, Int) = BSGSGroup.sift(g, base.view(j, k), transversals.view(j, k))

  def sift(g: P, j: Int = 0): (P, Int) = BSGSGroup.sift(g, base.drop(j), transversals.drop(j))
}

object BSGSGroup {
  def sift[P <: Permutation[P], T <: Transversal[P]](g: P, base: Iterable[Int], transversals: Iterable[T]): (P, Int) = {
    var siftee = g
    var k = 0
      (base, transversals).zipped map ( (b, Ui) => {
        val beta = b**siftee
        if (!Ui.contains(beta))
          return (siftee, k)
        siftee = siftee * Ui(beta)
        k += 1
      } )
    (siftee, k)
  }
  def siftEx[P <: Permutation[P], T <: Transversal[P]](g: P, base: Iterable[Int], transversals: Iterable[T]): (P, List[P], Int) = {
    var siftee = g
    var k = 0
    var lprod = List.empty[P]
      (base, transversals).zipped map ( (b, Ui) => {
        val beta = b**siftee
        if (!Ui.contains(beta))
          return (siftee, lprod, k)
        lprod = Ui(beta) :: lprod
        siftee = siftee * Ui(beta)
        k += 1
      } )

    (siftee, lprod, k)
  }
}
 */
