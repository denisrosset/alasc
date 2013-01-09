package com.faacets.perm

import Implicits._

object BaseConstruction {
    import scala.collection.mutable.ArrayBuffer

  def chooseBaseElement(h: Permutation, base: Seq[Domain]): Option[Int] = {
    for (beta <- 0 until h.size if !base.contains(beta))
      if (h.hasInSupport(beta))
        return Some(beta)
    return None
  }

  def setup(prescribedBase: Seq[Domain], generators: Seq[Permutation]): (ArrayBuffer[Domain], ArrayBuffer[Transversal], ArrayBuffer[List[Permutation]]) = {
    val B = ArrayBuffer.empty[Domain] ++ prescribedBase
    val U = ArrayBuffer.empty[Transversal]
    val S = ArrayBuffer.empty[List[Permutation]]
    val n = generators.head.domainSize
    val nonIdentityGenerators = generators.filter(!_.isIdentity)
    // extend base so that no group element fixes all base elements
    for (g <- nonIdentityGenerators) {
      if (!B.exists(beta => g.hasInSupport(beta))) {
        chooseBaseElement(g, B) match {
          case Some(beta) => B += beta
          case None => { }
        }
      }
    }
    if (B.isEmpty) {
      B += 0
      U += new TrivialTransversal(0, n)
      S += List.empty[Permutation]
      return (B, U, S)
    }
    for ((b,i) <- B.view.zipWithIndex) {
      val Si = nonIdentityGenerators.filter(g => !(0 until i).exists(j => g.hasInSupport(B(j))))
      U += ExplicitTransversal.fromGenerators(b, Si)
      S += Si.toList
    }
    return (B, U, S)
  }
}

