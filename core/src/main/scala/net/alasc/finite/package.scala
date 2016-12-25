package net.alasc

import net.alasc.algebra.PermutationAction

package object finite {

  type FaithfulPermutationActionBuilder[A] = FaithfulActionBuilder[A, Int, PermutationAction[A]]

  object FaithfulPermutationActionBuilder {
    def apply[A](implicit ev: FaithfulPermutationActionBuilder[A]): FaithfulPermutationActionBuilder[A] = ev
  }

}
