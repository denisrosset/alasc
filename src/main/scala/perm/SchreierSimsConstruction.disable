package com.faacets.perm

import scala.collection.mutable

object SchreierSimsConstruction {
  import scala.collection.mutable.ArrayBuffer

  def chooseBaseElement[P <: Permutation[P]](h: P, base: Base): Option[Int] = {
    for (beta <- 0 until h.domainSize if !base.contains(beta))
      if (h.hasInSupport(beta))
        return Some(beta)
    return None
  }

  def setup[P <: Permutation[P], T <: Transversal[P]](
    prescribedBase: Base,
    generators: Seq[P], 
    transversalFromGenerators: (Domain, Iterable[P], P) => T): (ArrayBuffer[Domain], ArrayBuffer[T], ArrayBuffer[List[P]]) = {
    val id = generators.head.identity
    val B = ArrayBuffer.empty[Domain] ++ prescribedBase
    val U = ArrayBuffer.empty[T]
    val S = ArrayBuffer.empty[List[P]]
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
      U += transversalFromGenerators(0, List.empty[P], id)
      S += List.empty[P]
      return (B, U, S)
    }
    for ((b,i) <- B.view.zipWithIndex) {
      val Si = nonIdentityGenerators.filter(g => !(0 until i).exists(j => g.hasInSupport(B(j))))
      U += transversalFromGenerators(b, Si, id)
      S += Si.toList
    }
    return (B, U, S)
  }

  def construct[P <: Permutation[P], T <: Transversal[P]](
    prescribedBase: Base,
    generators: Seq[P],
    transversalFromGenerators: (Domain, Iterable[P], P) => T): BSGSGroup[P, T] = {
    val tuple = setup[P, T](prescribedBase, generators, transversalFromGenerators)
    val id = generators.head.identity
    val B = tuple._1
    val U = tuple._2
    val S = tuple._3
    val n = generators.head.domainSize
    // we do not do the Schreier generator optimization
    var j = B.size
    while (j >= 1) {
      def iterateOverBaseAndGenerators: Boolean = {
        for(beta <- U(j-1).orbitIterator; x <- S(j-1)) {
          val el = (U(j-1)(beta).inverse)*x*U(j-1)(beta**x)
          // sift for S_{j+1} so use index j here
          val (h, k) = BSGSGroup.sift(el, B.drop(j), U.drop(j))
          if (k < B.size - j  || !h.isIdentity) {
            if (j == B.size) {
              chooseBaseElement(h, B) match {
                case Some(gamma) => B += gamma
                case None => { assert(false) }
              }
              assert(j < B.size)
              S += List.empty[P]
              U += transversalFromGenerators(B(U.size), List.empty[P], id)
            }
            S(j) = h :: S(j)
            // TODO: more efficient U(j) = U(j).orbitUpdate(j, S(j), h)
            U(j) = transversalFromGenerators(B(j), S(j), id)
            j += 1
            return false
          }
        }
        return true
      }
      if (iterateOverBaseAndGenerators)
        j -= 1
    }
    val SGS = mutable.HashSet.empty[P]
    for (s <- S) SGS ++= s
    return new BSGSGroup(B.toList, SGS.toList, U.toList)
  }
}
