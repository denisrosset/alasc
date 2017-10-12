package net.alasc.perms

package object sized {

  object instances extends SizedInstances
  object syntax extends SizedSyntax
  object implicits extends SizedInstances with SizedSyntax

  type Perm16 = Long { type Tag = net.alasc.perms.sized.Perm16.type }

}
