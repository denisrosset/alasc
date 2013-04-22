package com.faacets.perm
package wreath

/** Represents the 'inhomogenous' wreath product (h, a_0...a_n-1), where h is a permutation group and
  * a_0 ... a_n-1 are finite groups. 
  */
trait InhWreathProductGroup extends FiniteGroup {
  import scala.util.Random

  type Group <: InhWreathProductGroup
  type Element <: InhWreathProductElement

  type H <: PermutationGroup
  type A <: FiniteGroup
  val h: H
  val avec: Vector[A]

  def assertValid {
    h.assertValid
    for (g <- h.generators) {
      val permavec = g.images.map(avec(_))
      assert(avec == permavec)
    }
    avec.foreach(_.assertValid)
  }

  def identity = make(
    h.identity: h.Element,
    avec.map(_.identity))

  def generatorsIterator =
    // Generators of the extern permutation
    (for (gen <- h.generatorsIterator) yield
      identity.copy(hel = gen)) ++
  // Generators of the intern permutations for the k-th copy of group a
  (for ((a, k) <- avec.zipWithIndex;
    id = identity;
    gen <- a.generatorsIterator) yield
    id.copy(aelvec = id.aelvec.updated(k, gen)))

  def contains(e: Element) = h.contains(e.hel) && (e.aelvec zip avec).forall {
    case (ael, a) => a.contains(ael.asInstanceOf[a.Element])
  }

  def order = h.order * avec.map(a => a.order).product

  def elementsIterator = for(hel <- h.elementsIterator; // loop over iterator on h elements
    aels <- combine(avec.map(_.elements)).iterator) // with iterators on the each copy of a
  yield make(hel, aels.toVector)

  def randomElement()(implicit gen: Random = Random) = make(
    h.randomElement,
    avec.map( _.randomElement ))

  def make(hel: h.Element, aelvec: Vector[A#Element]): Element

  trait InhWreathProductElement extends FiniteGroupElement {
    self: Element =>
    val hel: h.Element
    val aelvec: Vector[A#Element]

    def copy(hel : h.Element = this.hel, aelvec : Vector[A#Element] = this.aelvec): Element
    def assertValid {
      hel.assertValid
      aelvec.foreach( _.assertValid )
    }

    def *(that: Element) =
      make(
        hel * that.hel,
        aelvec.indices.map(
          k => {
            val el1 = aelvec(that.hel.inverse.image(k))
            val el11 = el1.asInstanceOf[el1.group.Element]
            val el2 = that.aelvec(k).asInstanceOf[el1.group.Element]
            val el3 = el11 * el2
            el3.asInstanceOf[A#Element]
          }).toVector)

    def inverse = make(
      hel.inverse,
      Vector(aelvec.indices.map(
        k => aelvec(hel.image(k)).inverse):_*))

    def isIdentity = hel.isIdentity && aelvec.forall(_.isIdentity)
    def equal(that: Element) = hel == that.hel && aelvec == that.aelvec
  }
}
