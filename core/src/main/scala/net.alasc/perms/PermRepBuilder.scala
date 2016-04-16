package net.alasc.perms

trait PermRepBuilder[G] {

  def build(generators: Iterable[G]): PermRep[G]

}
