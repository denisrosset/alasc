package com.faacets.perm

/** A transversal is defined by a set of permutations S and an element beta.
  * 
  * The transversal stores the orbit of beta under <S> and can retrieve for any
  * element b in the orbit the product u_b = s_i1 s_i2 ... of elements of S such
  * that beta ** u_b = b.
  */
trait Transversal[P <: Permutation[P], T <: Transversal[P, T]] {
  override def toString: String = (for (key <- orbitIterator) yield key + " => " + apply(key)).mkString("","\n","")
  def beta: Domain /** Element for which the transversal is defined. */
  def contains(el: Domain): Boolean /** Tests if el is in the orbit of beta. */
  def orbitIterator: Iterator[Domain] /** Iterator on the orbit of beta. */
  def elementsIterator: Iterator[P] /** Iterator on the transversal elements u_b. */
  def apply(el: Domain): P /** Retrieves the transversal corresponding to the element el. */
  def size: Int /** Number of elements in the orbit of beta. */
  def randomElement: P = elementsIterator.drop(scala.util.Random.nextInt(size)).next() /** Returns a random element of the transversal. */
  def assertValid /** Checks the sanity of the transversal. */ {
    for (b <- orbitIterator)
      assert (apply(b).image(beta) == b)
  }
  def +(s: P): T /** Returns a new transversal extended with s added to S. */
}
