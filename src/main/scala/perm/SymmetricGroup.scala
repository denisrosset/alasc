package com.faacets.perm

/** Symmetric group defined on the domain 0 .. degree-1. */
case class SymmetricGroup(val degree: Int) extends ExplicitPermutationGroup {
  type Group = SymmetricGroup
  type Element = SymmetricGroupElement

  def make(img: Vector[Domain]) = SymmetricGroupElement(img, this)

  def assertValid = degree > 0
  def identity = make((0 until degree).toVector)
  /* For each cell of size k, we use as generators the k-1 shifts. */
  def generators =
    (0 to degree - 2).map(i => make((0 until degree).toVector.updated(i, i + 1).updated(i + 1, i)))
  def order = (1 to degree).foldLeft(BigInt(1))(_*_)
  /* The symmetric group contains all permutation of domainSize == degree. */
  def contains(e: Element): Boolean = true
  def elements = (0 until degree).toVector.permutations.map(make(_)).toIterable
  def randomElement =
    make(scala.util.Random.shuffle((0 until degree).toVector))

  case class SymmetricGroupElement(override val img: Vector[Domain], group: SymmetricGroup) extends ExplicitPermutation(img) { }
}
