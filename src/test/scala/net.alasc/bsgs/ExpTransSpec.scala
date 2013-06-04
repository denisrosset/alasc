package net.alasc
package bsgs

import org.scalacheck._

object ExpTransSpec extends Properties("ExpTrans") {
  import Dom.OneBased._

  val genBaseGenerators = for {
    k <- Gen.choose(3, 20)
    n <- Gen.choose(2, 5)
    beta <- Gen.choose(1, k)
  } yield (Dom._1(beta), (1 to n).map(a => Sym(k).random))

  property("contains") = Prop.forAll(genBaseGenerators) {
    Function.tupled( (beta, gens) => {
      val t = ExpTransBuilder.empty(beta, Perm(gens.head.size)).updated(gens, gens)
      t.contains(beta) && (for (b <- t.keysIterator; g <- gens) yield t.isDefinedAt(g.image(b)) && t.isDefinedAt(g.inverse.image(b))).forall(b => b)
    } )
  }
  
  property("u/uinv") = Prop.forAll(genBaseGenerators) {
    Function.tupled( (beta, gens) => {
      val t = ExpTransBuilder.empty(beta, Perm(gens.head.size)).updated(gens, gens)
      t.keysIterator.forall(b => t.u(b).image(beta) === b)
    } )
  }
}
