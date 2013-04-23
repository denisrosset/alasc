package com.faacets.perm
package bsgs

trait RandomSchreierSims extends BSGS {
  def prescribedBase: Base
  override val fullSubgroup = {
    val cons = BSGSConstruction.fromBase(prescribedBase)
    while (cons.order < underlyingGroup.order)
      cons += underlyingGroup.randomElement
    cons.asSubgroup
  }
}

trait NoPrescribedBase extends RandomSchreierSims {
  def prescribedBase: Base = Nil
}

trait PrescribeFullBase extends RandomSchreierSims {
  def prescribedBase = underlyingGroup.domain.toList
}
