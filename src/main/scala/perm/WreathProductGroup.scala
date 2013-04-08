package com.faacets.perm

/** Represents the wreath product a Wr h, where h is a permutation group and
  * a a finite group.
  */
case class WreathProductGroup[
  HE <: Permutation[HE], AE <: GroupElement[AE],
  H <: PermutationGroup[HE], A <: FiniteGroup[AE]](h: H, a: A)
    extends FiniteGroup[WreathProductElement[HE, AE]] {
  type WE = WreathProductElement[HE, AE]

  def assertValid {
    h.assertValid
    a.assertValid
  }

  def identity = WreathProductElement(
    h.identity,
    (0 until h.degree).toVector.map(i => a.identity))

  def generators = List(
    // Generators of the extern permutation
    for (gen <- h.generators) yield
      identity.copy(hel = gen),
    // Generators of the intern permutations for the k-th copy of group a
    for (k <- 0 until h.degree;
      id = identity;
      gen <- a.generators) yield
      id.copy(aelvec = id.aelvec.updated(k, gen))
  ).flatten

  def contains(e: WE) = h.contains(e.hel) && e.aelvec.forall(a.contains(_))

  def order = h.order * (0 until h.degree).map(i => a.order).product

  def iterator = for(hel <- h.iterator; // loop over iterator on h elements
    aels <- combine((0 until h.degree).toList.map(i => a))) // with iterators on the each copy of a
  yield WreathProductElement(hel, aels.toVector)

  def randomElement = WreathProductElement(
    h.randomElement,
    (0 until h.degree).toVector.map(i => a.randomElement))
}

case class WreathProductElement[HE <: Permutation[HE], AE <: GroupElement[AE]](hel: HE, aelvec: Vector[AE]) extends GroupElement[WreathProductElement[HE, AE]] {
  type WE = WreathProductElement[HE, AE]
  def assertValid {
    hel.assertValid
    aelvec.foreach( _.assertValid )
  }

  def *(that: WE) =
    WreathProductElement(
      hel * that.hel,
      Vector(aelvec.indices.map(
        k => aelvec(that.hel.inverse.image(k))*that.aelvec(k)):_*))

  def inverse = WreathProductElement(
    hel.inverse,
    Vector(aelvec.indices.map(
      k => aelvec(hel.image(k)).inverse):_*))

  def isIdentity = hel.isIdentity && aelvec.forall(_.isIdentity)
  def equal(that: WE) = hel == that.hel && aelvec == that.aelvec
}
