package net.alasc.perms
package chain

import spire.util.Opt

import net.alasc.bsgs

abstract class PermGrpChain[G] extends PermGrp[G] { lhs =>

  def chain: bsgs.Chain[G]

  def chainOpt: Opt[bsgs.Chain[G]]

}
