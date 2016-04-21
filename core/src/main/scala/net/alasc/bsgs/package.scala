package net.alasc

import net.alasc.algebra.PermutationAction

package object bsgs {

  implicit def richChain[G, F <: PermutationAction[G] with Singleton](chain: Chain[G, F]): RichChain[G, F] =
    new RichChain[G, F](chain)

  implicit def richMutableChain[G, F <: PermutationAction[G] with Singleton](mutableChain: MutableChain[G, F]): RichMutableChain[G, F] =
    new RichMutableChain[G, F](mutableChain.start)

}
