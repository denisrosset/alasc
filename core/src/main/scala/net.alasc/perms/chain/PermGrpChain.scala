package net.alasc.perms
package chain

import net.alasc.prep.bsgs
import spire.util.Opt

abstract class PermGrpChain[G] extends PermGrp[G] { lhs =>

  def chain: bsgs.Chain[G]

  def chainOpt: Opt[bsgs.Chain[G]]

}
