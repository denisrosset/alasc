package com.faacets.perm

class BSGSGroup[P <: Permutation[P], T <: Transversal[P]](B: List[Domain], S: List[P], U: List[T]) extends PermutationGroup[P] {
  val base: List[Domain] = B
  val strongGeneratingSet: List[P] = S
  val transversals: List[T] = U
  val degree: Int = S.head.domainSize

  override def order: Int = (transversals :\ 1)((kv:T, p: Int) => kv.size*p)
  override def generatingSet = S
  override def verify = true // to fix
  override def contains(perm: P) = sifts(perm)
  override def elements = List.empty[P] // to fix
  def sifts(g: P): Boolean = {
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
