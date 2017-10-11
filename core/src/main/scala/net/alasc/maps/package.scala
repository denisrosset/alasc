package net.alasc

import net.alasc.perms.Perm

package object maps {

  type PermMonomorphism[A] = Monomorphism[A, Perm]

}
