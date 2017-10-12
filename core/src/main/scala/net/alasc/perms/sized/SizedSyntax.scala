package net.alasc.perms.sized

trait SizedSyntax {

  implicit def perm16Ops(lhs: Perm16): Perm16Ops = new Perm16Ops(lhs)

}
