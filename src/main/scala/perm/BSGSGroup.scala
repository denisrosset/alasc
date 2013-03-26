package com.faacets.perm

class BSGSGroup[P <: Permutation[P], T <: Transversal[P]](B: List[Domain], S: List[P], U: List[T]) extends PermutationGroup[P] {
  val base: List[Domain] = B
  val strongGeneratingSet: List[P] = S
  val transversals: List[T] = U
  val degree: Int = S.head.domainSize
  def randomElement = {
    var g = S.head.identity
    for (u <- transversals) {
      if (u.size > 1) {
        val el = u.elementsIterator.toList(scala.util.Random.nextInt(u.size))
        g = el * g
      }
    }
    g
  }
  def iterator = {
    def iter(tlist: List[T]): Iterator[P] = tlist match {
      case head :: Nil => head.elementsIterator
      case head :: tail => for(g <- head.elementsIterator; h <- iter(tail)) yield g*h
      case Nil => List(S.head.identity).iterator
    }
    iter(transversals)
  }
  override def order: Int = (1 /: transversals)((p:Int, kv:T) => kv.size*p)
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