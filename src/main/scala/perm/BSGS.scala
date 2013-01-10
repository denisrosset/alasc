package com.faacets.perm

import Implicits._

class BSGSGroup(B: List[Domain], S: List[Permutation], U: List[Transversal]) extend PermutationGroup {
  val base: List[Domain] = B
  val strongGeneratingSet: List[Permutation] = S
  val transversals: List[Transversal] = U
  val degree: Int = S.head.domainSize

  override def order: Int = (transversals :\ 1)((kv:Transversal, p: Int) => kv.size*p)
  override def generatingSet = S
  override def elements = {

  }
  def sifts(g: Permutation): Boolean = {
    val (siftee, m) = sift(g)
    m == base.size && siftee.isIdentity
  }

  def sift(g: Permutation, j: Int, k: Int): (Permutation, Int) = BSGSGroup.sift(g, base.view(j, k), transversals.view(j, k))

  def sift(g: Permutation, j: Int = 0): (Permutation, Int) = BSGSGroup.sift(g, base.drop(j), transversals.drop(j))
}

object BSGSGroup {
  def sift(g: Permutation, base: Iterable[Int], transversals: Iterable[Transversal]): (Permutation, Int) = {
    var siftee = g
    var k = 0
      (base, transversals).zipped map ( (b, Ui) => {
        val beta = b**siftee
        if (!Ui.contains(beta))
          return (siftee, k)
        siftee = siftee * Ui(beta).inverse
        k += 1
      } )
    (siftee, k)
  }
  def siftEx(g: Permutation, base: Iterable[Int], transversals: Iterable[Transversal]): (Permutation, List[Permutation], Int) = {
    var siftee = g
    var k = 0
    var lprod = List.empty[Permutation]
      (base, transversals).zipped map ( (b, Ui) => {
        val beta = b**siftee
        if (!Ui.contains(beta))
          return (siftee, lprod, k)
        lprod = Ui(beta) :: lprod
        siftee = siftee * Ui(beta).inverse
        k += 1
      } )

    (siftee, lprod, k)
  }
}
