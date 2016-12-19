package net.alasc

import net.alasc.algebra.PermutationAction

package object bsgs {

  implicit def richChain[G, A <: PermutationAction[G] with Singleton](chain: Chain[G, A]): RichChain[G, A] =
    new RichChain[G, A](chain)

  implicit def richMutableChain[G, A <: PermutationAction[G] with Singleton](mutableChain: MutableChain[G, A]): RichMutableChain[G, A] =
    new RichMutableChain[G, A](mutableChain.start)

}
