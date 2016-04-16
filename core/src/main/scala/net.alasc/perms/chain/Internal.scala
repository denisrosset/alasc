package net.alasc.perms.chain

trait Internal[G, I] {

  def internal(g: G): I

  def external(i: I): G

}

object Internal {

  val identityInstance: Identity[Any] = new Identity[Any]

  final class Identity[G] extends Internal[G, G] {

    def internal(g: G): G = g

    def external(i: G): G = g

  }

  def identity[G] = identityInstance.asInstanceOf[Identity[G]]

}
