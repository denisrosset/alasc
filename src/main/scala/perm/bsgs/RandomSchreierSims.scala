package com.faacets.perm
package bsgs

trait RandomSchreierSims[G <: PermutationGroup] extends BSGS[G] {
  def prescribedBase: Base
  override val fullSubgroup = {
    val cons = BSGSConstruction.fromBase(prescribedBase)
    while (cons.order < underlyingGroup.order)
      cons += underlyingGroup.randomElement
    cons.asSubgroup
  }
}

trait NoPrescribedBase[G <: PermutationGroup] extends RandomSchreierSims[G] {
  def prescribedBase: Base = Nil
}

trait PrescribeFullBase[G <: PermutationGroup] extends RandomSchreierSims[G] {
  def prescribedBase = underlyingGroup.domain.toList
}
