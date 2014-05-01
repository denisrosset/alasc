package net.alasc

abstract class SubgroupSearchTest[F <: Finite[F]] {
  def test(baseImage: Dom, deltaP: Dom, action: Action[F], uPrev: F, transversal: Transversal[F]): SubgroupSearchTest[F]
}

case class TrivialSubgroupSearchTest[F <: Finite[F]]() extends SubgroupSearchTest[F] {
  def test(baseImage: Dom, deltaP: Dom, action: Action[F], uPrev: F, transversal: Transversal[F]): SubgroupSearchTest[F] = this
}
