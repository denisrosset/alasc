package com.faacets.perm

import scala.collection.mutable.ArrayBuffer

/** A mutable class describing a BSGS construction.
  * @param S    Members of the strong generating set fixing base points
  *             until level k for k = 0 .. m - 1.
  * @param U    Transversal for the each level.
  */
class BSGSConstruction[P <: Permutation[P], T <: Transversal[P, T]]
  (val identity: P, val S: ArrayBuffer[List[P]], val U: ArrayBuffer[T]) {

  val m = U.length /** Length of the stabilizer chain. */

  def order: Int = (1 /: U)( (p:Int, u:T) => u.size*p)

  def sift(g: P, i: Int = 0): (P, Int) = {
    // we left the base? exit
    if (i >= m)
      return (g, m)
    val b = g.image(U(i).beta)
    // is the image of the current base element in the transversal ?
    if (!U(i).contains(b))
      // if not, exit
      return (g, i)
    // we fixed the current base element, on to the next one
    sift(g * U(i)(b), i + 1)
  }

  def addElement(g: P, emptyU: (Domain, P) => T) = {
    var newGenerator = false
    val (h, j) = sift(g)
    if (j < m) // new strong generator at level j
      newGenerator = true
    else if (!h.isIdentity) { // new strong generator h fixes all the base points
      newGenerator = true
      val b = (0 until h.domainSize).find( h.hasInSupport(_) ).get
      S += List.empty[P]
      U += emptyU(b, identity)
    }
    if (newGenerator) {
      for (l <- 1 to j) {
        S(l) = h :: S(l)
        U(l) = U(l) + h
      }
    }
  }

}

object BSGSConstruction {
  def completeBase[P <: Permutation[P]](base: Vector[Domain], X: Seq[P]) = {
    var compBase = base
    var isComplete = true
    assert(X.forall(!_.isIdentity))
    do {
      isComplete = true
      X.find(x => compBase.forall( b => x.image(b) == b )) match {
        case Some(x) => {
          compBase = compBase :+ (0 until x.domainSize).find( k => x.image(k) != k ).get
          isComplete = false
        }
        case None => { }
      }
    } while (!isComplete)
    compBase
  }

  def setup[P <: Permutation[P], T <: Transversal[P, T]]
    (id: P, candBase: Vector[Domain], X: Seq[P], emptyU: (Domain, P) => T) = {
    val base = completeBase(candBase, X)
    // We compute the candidate generating sets for the stabilizer chain
    val S = ArrayBuffer((0 to base.length).map( i => X.filter( x => base.take(i).forall( e => x.image(e) == e ) ).toList ):_*)
    val U = ArrayBuffer((0 until base.length).map( i => S(i).foldLeft(emptyU(base(i), id))( _+_ ) ):_*)
    new BSGSConstruction[P, T](id, S, U)
  }

  def setupWithFullBase[P <: Permutation[P], T <: Transversal[P, T]]
    (id: P, X: Seq[P], emptyU: (Domain, P) => T) = {
    val base = (0 until id.domainSize).toVector
    // We compute the candidate generating sets for the stabilizer chain
    val S = ArrayBuffer((0 to base.length).map( i => X.filter( x => base.take(i).forall( e => x.image(e) == e ) ).toList ):_*)
    val U = ArrayBuffer((0 until base.length).map( i => S(i).foldLeft(emptyU(base(i), id))( _+_ ) ):_*)
    new BSGSConstruction[P, T](id, S, U)
  }

  def randomSchreierSims[P <: Permutation[P], T <: Transversal[P, T]]
    (id: P, cons: BSGSConstruction[P, T], random: () => P, order: Int, emptyU: (Domain, P) => T) = {
    while (cons.order < order)
      cons.addElement(random(), emptyU)
    new BSGSGroup(id, cons.S.flatten.toSet.toSeq, cons.U.toVector)
  }
}

