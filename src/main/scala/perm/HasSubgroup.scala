package com.faacets.perm

trait AbstractHasSubgroup extends AbstractGroup {
  superGroup =>
  type Subgroup <: AbstractSubgroup /** Type of a subgroup of this group. */

  trait AbstractSubgroup extends AbstractGroup {
    subGroup: Subgroup =>
    type Group <: AbstractSubgroup
    type Element <: AbstractSubgroupElement
    val mySuperGroup: superGroup.Group /** Group this is subgroup of. */

    trait AbstractSubgroupElement extends AbstractElement {
      element: subGroup.Element =>
      def superElement: superGroup.Element
    }
  }
}

trait FiniteHasSubgroup extends AbstractHasSubgroup with FiniteGroup {
  superGroup =>
  type Subgroup <: FiniteSubgroup

  trait FiniteSubgroup extends AbstractSubgroup with FiniteGroup {
    subGroup: Subgroup =>
    type Group <: FiniteSubgroup
    type Element <: FiniteSubgroupElement

    trait FiniteSubgroupElement extends AbstractSubgroupElement with FiniteGroupElement {
      element: subGroup.Element =>
    }
  }
}

trait PermutationHasSubgroup extends FiniteHasSubgroup with PermutationGroup {
  superGroup =>
  type Subgroup <: PermutationSubgroup
  trait PermutationSubgroup extends FiniteSubgroup with PermutationGroup {
    subGroup: Subgroup =>

    type Group <: PermutationSubgroup
    type Element <: PermutationSubgroupElement

    def degree = superGroup.degree
    trait PermutationSubgroupElement extends FiniteSubgroupElement with PermutationElement {
      element: subGroup.Element =>
    }
  }
}
