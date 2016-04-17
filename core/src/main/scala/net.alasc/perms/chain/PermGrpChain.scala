package net.alasc.perms
package chain

import spire.algebra.{Eq, Group}

import net.alasc.algebra.FaithfulPermutationAction
import net.alasc.prep.bsgs
import spire.util.Opt

abstract class PermGrpChain[G] extends PermGrp[G] {

  implicit def builder: PermGrpChainBuilder[G]

  def chain: bsgs.Chain[G]

  def chainOpt: Opt[bsgs.Chain[G]]

}
