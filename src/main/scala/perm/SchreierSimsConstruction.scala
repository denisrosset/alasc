package com.faacets.perm

import scala.collection.mutable
import Implicits._

object SchreierSimsConstruction {
  def construct(prescribedBase: Base, generators: Seq[Permutation]): BSGS = {
    val tuple = BaseConstruction.setup(prescribedBase, generators)
    val B = tuple._1
    val U = tuple._2
    val S = tuple._3
    val n = generators.head.domainSize
    // we do not do the Schreier generator optimization
    var j = B.size
    while (j >= 1) {
      def iterateOverBaseAndGenerators: Boolean = {
        for(beta <- U(j-1).iterable; x <- S(j-1)) {
          val el = U(j-1)(beta)*x*(U(j-1)(beta**x).inverse)
          // sift for S_{j+1} so use index j here
          val (h, k) = BSGS.sift(el, B.drop(j), U.drop(j))
          if (k < B.size - j  || !h.isIdentity) {
            if (j == B.size) {
              BaseConstruction.chooseBaseElement(h, B) match {
                case Some(gamma) => B += gamma
                case None => { assert(false) }
              }
              assert(j < B.size)
              S += List.empty[Permutation]
              U += new TrivialTransversal(B(U.size), n)
            }
            S(j) = h :: S(j)
            // TODO: more efficient U(j) = U(j).orbitUpdate(j, S(j), h)
            U(j) = ExplicitTransversal.fromGenerators(B(j), S(j))
            j += 1
            return false
          }
        }
        return true
      }
      if (iterateOverBaseAndGenerators)
        j -= 1
    }
    val SGS = mutable.HashSet.empty[Permutation]
    for (s <- S) SGS ++= s
    return new BSGSGroup(B.toList, SGS.toList, U.toList, n)
  }
}
