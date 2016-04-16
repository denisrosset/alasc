package net.alasc.perms
package chain

import spire.algebra.{Eq, Group}

import net.alasc.algebra.FaithfulPermutationAction
import net.alasc.prep.bsgs
import spire.util.Opt

abstract class PermGrpChain[G, I] extends PermGrp[G] {

  implicit def builder: PermGrpChainBuilder[G, I]

  implicit def internal: Internal[G, I]

  implicit def groupI: Group[I]

  implicit def equI: Eq[I]

  implicit def permutationActionI: FaithfulPermutationAction[I]

  def chain: bsgs.Chain[I]

  def chainOpt: Opt[bsgs.Chain[I]]

}
