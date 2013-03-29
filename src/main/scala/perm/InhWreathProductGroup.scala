package com.faacets.perm

/** Represents the 'inhomogenous' wreath product (h, a_0...a_n-1), where h is a permutation group and
  * a_0 ... a_n-1 are finite groups. 
  */
case class InhWreathProductGroup[
  H <: PermutationGroup[HE], A <: FiniteGroup[AE],
  HE <: Permutation[HE], AE <: GroupElement[AE]](h: H, as: Vector[A])
    extends FiniteGroup[InhWreathProductElement[HE, AE]] {
  type E = InhWreathProductElement[HE, AE]

  def assertValid {
    h.assertValid
    for (g <- h.generators) {
      val permas = g.images.map(as(_))
      assert(as == permas)
    }
    as.foreach(_.assertValid)
  }

  def identity = InhWreathProductElement(
    h.identity,
    as.map(_.identity))

  def generators = List(
    // Generators of the extern permutation
    for (gen <- h.generators) yield
      identity.copy(hel = gen),
    // Generators of the intern permutations for the k-th copy of group a
    for ((a, k) <- as.zipWithIndex;
      id = identity;
      gen <- a.generators) yield
      id.copy(aelvec = id.aelvec.updated(k, gen))
  ).flatten

  def contains(e: E) = h.contains(e.hel) && (e.aelvec zip as).forall( aela => aela._2.contains(aela._1))

  def order = h.order * as.map(a => a.order).product

  def iterator = for(hel <- h.iterator; // loop over iterator on h elements
    aels <- combine(as)) // with iterators on the each copy of a
  yield InhWreathProductElement(hel, aels.toVector)

  def randomElement = InhWreathProductElement(
    h.randomElement,
    as.map( _.randomElement ))
}

case class InhWreathProductElement[HE <: Permutation[HE], AE <: GroupElement[AE]](hel: HE, aelvec: Vector[AE]) extends GroupElement[InhWreathProductElement[HE, AE]] {
  type E = InhWreathProductElement[HE, AE]
  def assertValid {
    val aelvecperm = hel.images.map(aelvec(_))
    // test that permuted elements are compatible
    for ((a1,a2) <- aelvec zip aelvecperm) a1 * a2
    hel.assertValid
    aelvec.foreach( _.assertValid )
  }

  def *(that: E) =
    InhWreathProductElement(
      hel * that.hel,
      Vector(aelvec.indices.map(
        k => aelvec(that.hel.inverse.image(k))*that.aelvec(k)):_*))

  def inverse = InhWreathProductElement(
    hel.inverse,
    Vector(aelvec.indices.map(
      k => aelvec(hel.image(k)).inverse):_*))

  def isIdentity = hel.isIdentity && aelvec.forall(_.isIdentity)
  def equal(that: E) = hel == that.hel && aelvec == that.aelvec
}
