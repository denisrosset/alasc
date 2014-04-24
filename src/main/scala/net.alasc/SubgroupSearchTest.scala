package net.alasc

trait SubgroupSearchTest[F <: Finite[F]] extends AnyRef {
  def apply(baseImage: Dom, deltaP: Dom, act: Action[F], uPrev: F, transversal: Transversal[F]): Option[SubgroupSearchTest[F]]
}

case class TrivialSubgroupSearchTest[F <: Finite[F]]() extends SubgroupSearchTest[F] {
  def apply(baseImage: Dom, deltaP: Dom, act: Action[F], uPrev: F, transversal: Transversal[F]) = Some(this)
}
