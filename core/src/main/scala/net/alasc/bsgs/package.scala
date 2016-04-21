package net.alasc

import net.alasc.algebra.FaithfulPermutationAction

package object bsgs {

  implicit def richChain[G, F <: FaithfulPermutationAction[G] with Singleton](chain: Chain[G, F]): RichChain[G, F] =
    new RichChain[G, F](chain)

  implicit def richMutableChain[G, F <: FaithfulPermutationAction[G] with Singleton](mutableChain: MutableChain[G, F]): RichMutableChain[G, F] =
    new RichMutableChain[G, F](mutableChain.start)

}
