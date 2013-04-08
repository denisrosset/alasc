package com.faacets.perm

/** Symmetric group defined on the domain 0 .. degree-1. */
case class SymmetricGroup(val degree: Int) 
    extends PermutationGroup[ExplicitPermutation] {
  def assertValid = degree > 0
  def identity = ExplicitPermutation(degree)
  /* For each cell of size k, we use as generators the k-1 shifts. */
  def generators =
    ((0 until degree - 1) zip (1 until degree)).map {
      case ((i,j)) => ExplicitPermutation(degree)(i,j) }
  def order = (1 to degree).foldLeft(BigInt(1))(_*_)
  /* The symmetric group contains all permutation of domainSize == degree. */
  def contains(perm: ExplicitPermutation): Boolean = true
  def iterator = (0 until degree).toVector.permutations.map(i => new ExplicitPermutation(i))
  def randomElement =
    new ExplicitPermutation(scala.util.Random.shuffle((0 until degree).toVector))
}
