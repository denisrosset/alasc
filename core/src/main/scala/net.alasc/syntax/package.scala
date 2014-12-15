package net.alasc

package object syntax {
  object check extends CheckSyntax
  object monoid extends MonoidSyntax
  object sequence extends SequenceSyntax
  object finiteGroup extends FiniteGroupSyntax
  object permutationAction extends PermutationActionSyntax
  object shiftablePermutation extends ShiftablePermutationSyntax
  object subgroup extends SubgroupSyntax
  object permutationSubgroup extends PermutationSubgroupSyntax
  object withBase extends WithBaseSyntax
  object partialMonoidWithBase extends PartialMonoidWithBaseSyntax
  object all extends AllSyntax
}
