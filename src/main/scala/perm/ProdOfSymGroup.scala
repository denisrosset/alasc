package com.faacets.perm

/** Permutation group defined on the a partition of a subset of the domain 0 .. degree-1.
  * The subset is partitioned into cells, and the permutation group is the product of symmetric
  * groups each acting on a cell.
  * 
  * @param degree   The degree of the group, i.e. the extent of the domain.
  * @param part     A vector of cells, each cell is written as a vector of
  *                 domain elements.
  */
case class ProdOfSymGroup(degree:Int, part: Vector[Vector[Domain]]) 
    extends PermutationGroup[ExplicitPermutation] {
  def assertValid {
    /* Checks that the cells do not intersect, and that all cell elements
     * are in the domain. */
    for (i <- 1 until part.length) {
      for (j <- 0 until i - 1)
        assert(part(i).intersect(part(j)).isEmpty)
      for (j <- part(i))
        assert(0 <= j && j < degree)
    }
  }
  def identity = ExplicitPermutation(degree)
  /* For each cell of size k, we use as generators the k-1 shifts. */
  def generators =
    part.map( p => (p zip p.tail).map { case ((i,j)) => ExplicitPermutation(degree)(i,j) } ).flatten
  /* The order is the product of fact(n_i), where n_i is the size of the cells. */
  def order = part.map { i:Vector[Domain] => (1 to i.length).product }.product
  /* Checks that the permutation maps each cell to itself set-wise, and does not
     move points that are not in a call. */
  def contains(perm: ExplicitPermutation): Boolean = {
    for (i <- 0 until degree) {
      val j = perm.image(i)
      if (i != j) {
        part.find(_.contains(j)) match {
          case None => return false
          case Some(p) => if(!p.contains(i)) return false
        }
      }
    }
    true
  }

  /* Returns the permutation where each cell has specified images. */
  def withImages(img: Seq[Seq[Domain]]) = {
    var images = Array((0 until degree):_*)
    for ((p, s) <- (part zip img)) {
      for ((i, j) <- (p zip s))
        images(i) = j
    }
    new ExplicitPermutation(images.toVector)
  }

  /* Iterates through the group elements by constructing explicitly the permutations
   * of each cell, then taking the cartesian product. */
  def iterator = combine(part.map(_.permutations.toList)).map(withImages(_)).toIterator
  /* Gives a random element by selecting random images for each cell in the partition. */
  def randomElement =
    withImages(part.map(scala.util.Random.shuffle(_)))
}

object ProdOfSymGroup {
  def leaveInvariant[D](els: Seq[D]) = {
    val part = els.toVector.zipWithIndex. // pair elements with their indices
      groupBy(_._1).values. // group equal elements
      map(_.map(_._2)). // retrieve only the index of elements
      filter(_.length > 1).toVector // retrieve only groups of size > 1
    ProdOfSymGroup(els.length, part)
  }
}
