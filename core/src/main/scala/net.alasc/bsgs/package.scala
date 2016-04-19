package net.alasc

import spire.util.Opt

package object bsgs {

  implicit def richChain[G](chain: Chain[G]): RichChain[G] =
    new RichChain[G](chain)

  implicit def richMutableChain[G](mutableChain: MutableChain[G]): RichMutableChain[G] =
    new RichMutableChain[G](mutableChain.start)

  implicit def richBaseGuideOpt(baseGuideOpt: Opt[BaseGuide]): RichBaseGuideOpt =
    baseGuideOpt match {
      case Opt(baseGuide) => new RichBaseGuideOpt(baseGuide)
      case _ => new RichBaseGuideOpt(null)
    }

}
