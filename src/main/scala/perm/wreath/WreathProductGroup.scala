package com.faacets.perm
package wreath

/** Represents the wreath product a Wr h, where h is a permutation group and
  * a a finite group.
  */
trait WreathProductGroup extends FiniteGroup {
  import scala.util.Random

  type Group <: WreathProductGroup
  type Element <: WreathProductElement

  type H <: PermutationGroup
  type A <: PermutationGroup
  val h: H
  val a: A
  def assertValid { h.assertValid; a.assertValid }

  def identity = make(
    h.identity,
    (0 until h.degree).toVector.map(i => a.identity))

  def generatorsIterator =
    // Generators of the extern permutation
    (for (gen <- h.generatorsIterator) yield
      identity.copy(hel = gen)) ++
  // Generators of the intern permutations for the k-th copy of group a
  (for (k <- 0 until h.degree;
    id = identity;
    gen <- a.generatorsIterator) yield
    id.copy(aelvec = id.aelvec.updated(k, gen))
  )

  def contains(e: Element) = h.contains(e.hel) && e.aelvec.forall(a.contains(_))

  def order = h.order * (0 until h.degree).map(i => a.order).product

  def elementsIterator = for(hel <- h.elementsIterator; // loop over iterator on h elements
    aels <- combine((0 until h.degree).toList.map(i => a.elements))) // with iterators on the each copy of a
  yield make(hel, aels.toVector)

  def randomElement()(implicit gen: Random = Random) = make(
    h.randomElement,
    (0 until h.degree).toVector.map(i => a.randomElement))

  def make(hel: h.Element, aelvec: Vector[a.Element]): Element

  trait WreathProductElement extends FiniteGroupElement {
    self: Element =>
    val hel: h.Element
    val aelvec: Vector[a.Element]

    def copy(hel : h.Element = this.hel, aelvec : Vector[a.Element] = this.aelvec): Element
    def assertValid { hel.assertValid; aelvec.foreach( _.assertValid ); }

    def *(that: Element) =
      make(
        hel * that.hel,
        Vector(aelvec.indices.map(
          k => aelvec(that.hel.inverse.image(k))*that.aelvec(k)):_*))

    def inverse = make(
      hel.inverse,
      Vector(aelvec.indices.map(
        k => aelvec(hel.image(k)).inverse):_*))

    def isIdentity = hel.isIdentity && aelvec.forall(_.isIdentity)
    def equal(that: Element) = hel == that.hel && aelvec == that.aelvec
  }
}
