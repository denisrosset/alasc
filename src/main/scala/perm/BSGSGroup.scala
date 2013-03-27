package com.faacets.perm

/** A class describing a level of the BSGS construction.
  * @param beta Element of the base for the current level
  * @param X    Members of the strong generating set fixing the base
  *             until the current level, and moving the current beta
  * @param u    Transversal for the current level
  */
case class BSGSLevel[P <: Permutation[P], T <: Transversal[P]]
  (beta: Domain, X: List[P], u: T) {
  /** Verifies the sanity of the current level.
    * @param prevBase Sequence of the base elements for the levels
    *                 up to the current one (optional).
    * @param nextS    Members of the strong generating set for all the
    *                 next levels
    * 
    */
  def assertValid(prevBase: Seq[Domain] = List.empty[Domain],
    nextS: Seq[P]) = {
    for(x <- X) {
      for (b <- prevBase)
        assert(x.image(b) == b)
      assert(x.image(beta) != beta)
    }
    for(b <- u.orbitIterator; x <- X)
      assert(u.contains(x.image(b)))
    for(b <- u.orbitIterator; s <- nextS)
      assert(u.contains(s.image(b)))
    u.assertValid
  }
}

object BSGSLevel {
  def empty[P <: Permutation[P], T <: Transversal[P]](
    beta: Domain,
    id: P,
    emptyTransversal: (Domain, P) => T) =
    new BSGSLevel(beta, List.empty[P], emptyTransversal(beta, id))
}

/** A permutation group with its stabilizer chain described by a base, a strong generating set,
  * and transversals.
  * The underlying permutation type is parametrized by P, and the type of transversals
  * is parametrized by T.
  * One should probably use the Schreier-Sims construction instead of constructing the BSGSGroup
  * object directly.
  */
case class BSGSGroup[P <: Permutation[P], T <: Transversal[P]]
  (identity: P, levels: Vector[BSGSLevel[P, T]]) extends PermutationGroup[P] {
  val degree = identity.domainSize
  val m = levels.length /** Length of the stabilizer chain. */

  def generatingSet = levels.map(_.X).flatten
  def order: Int = (1 /: levels)((p:Int, l:BSGSLevel[P, T]) => l.u.size*p)
  def contains(perm: P): Boolean = ???

  /** Checks this BSGS construction for consistency */
  def assertValid {
  
  }

  /** Produces a random element */
  def randomElement = {
    var g = identity
    for (l <- levels)
      g = l.u.randomElement * g
    g
  }

  /** Iterates through all the elements of the group, by describing any element
    * of the group as a product of members of transversals.
    */
  def iterator = {
    def iter(i: Int): Iterator[P] = {
      if (i == m) return List(identity).iterator
      if (i == m - 1) return levels(i).u.elementsIterator
      for(g <- levels(i).u.elementsIterator; h <- iter(i+1)) yield g*h
    }
    iter(0)
  }

  def sift(g: P, i: Int = 0): (P, Int) = {
    // we left the base? exit
    if (i >= m)
      return (g, m)
    val b = g.image(levels(i).beta)
    // is the image of the current base element in the transversal ?
    if (!levels(i).u.contains(b))
      // if not, exit
      return (g, i)
    // we fixed the current base element, on to the next one
    sift(g * levels(i).u(b), i + 1)
  }
/**
  def +(g: P): BSGSGroup[P, T] = {
    // sifts the new element
    val (h, i) = sift(g, 0)


    if (h.isIdentity) // if the element sifts, it is already a member
      return this

    if (i == m) // if we sift through the base, and aren't left with identity
      val newbase = base ++ Vector((0 until degree).find(i => h(i) != i).get)

  }*/
}
import scala.collection.immutable.TreeMap

object BSGSGroup {
  def emptyFullBase[P <: Permutation[P], T <: Transversal[P]](
    id: P,
    emptyTransversal: (Domain, P) => T) =
    BSGSGroup(
      id,
      (0 until id.domainSize).map(BSGSLevel.empty(_, id, emptyTransversal)).toVector
    )
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
