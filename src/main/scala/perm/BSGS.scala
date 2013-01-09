package com.faacets.perm

import Implicits._

class BSGS(B: List[Domain], S: List[Permutation], U: List[Transversal], n: Int) {
  val base: List[Domain] = B
  val strongGeneratingSet: List[Permutation] = S
  val transversals: List[Transversal] = U
  val degree: Int = n

  def order: Int = (transversals :\ 1)((kv:Transversal, p: Int) => kv.size*p)

  def sifts(g: Permutation): Boolean = {
    val (siftee, m) = sift(g)
    m == base.size && siftee.isIdentity
  }

  def sift(g: Permutation, j: Int, k: Int): (Permutation, Int) = BSGS.sift(g, base.view(j, k), transversals.view(j, k))

  def sift(g: Permutation, j: Int = 0): (Permutation, Int) = BSGS.sift(g, base.drop(j), transversals.drop(j))
}

object BSGS {
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
