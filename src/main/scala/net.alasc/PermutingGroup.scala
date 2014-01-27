package net.alasc

trait GenPermutingGroup extends GenFiniteGroup {
  /** Degree of this permutation group, i.e. size of its domain. */
  def degree: Int
  /** Domain of this permutation group. */
  def domain: Iterable[Dom]
}

trait PermutingGroup[P <: Permuting[P]] extends FiniteGroup[P] with GenPermutingGroup

trait GenPermutingGroupLike extends GenPermutingGroup {
  def domain = Dom.domain(degree)
}

trait PermutingGroupLike[P <: Permuting[P]] extends PermutingGroup[P] with GenPermutingGroupLike

