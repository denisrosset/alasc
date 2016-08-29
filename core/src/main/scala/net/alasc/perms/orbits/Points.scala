package net.alasc.perms
package orbits

import spire.algebra.Order
import spire.syntax.action._
import spire.syntax.cfor._
import spire.util.Opt

import net.alasc.algebra.PermutationAction
import net.alasc.util.GenericArray

object Points {

  def isSmallestInOrbit[G](point: Int, generators: Iterable[G])
                          (implicit G: PermutationAction[G], order: Order[Int]): Boolean =
    isSmallestInOrbit(point, generators, Opt.empty[MutableOrbit])(G, order)

  /** Tests whether `point` is the smallest point in its orbit under `generators`. An optional
    * empty [[MutableOrbit]] can be provided; it will be cleared when the function returns.
    *
    * @param emptyMutableOrbit (Optional) Provided empty mutable orbit to avoid allocations
    */
  def isSmallestInOrbit[G](point: Int, generators: Iterable[G], emptyMutableOrbit: Opt[MutableOrbit])
                          (implicit G: PermutationAction[G], order: Order[Int]): Boolean = {
    val orbit = emptyMutableOrbit match {
      case Opt(o) => o
      case _ => MutableOrbit.empty
    }
    val generatorArray = GenericArray[G](generators)
    orbit.addNew(point)
    var start = point
    while (start != -1) {
      // we want to find a smaller point
      cforRange(0 until generatorArray.length) { gi =>
        val g = generatorArray(gi)
        var el = start
        var existsSmaller = false
        while (el != -1 && !existsSmaller) {
          val image = el <|+| g
          if (!orbit.inOrbit(image)) {
            orbit.addNew(image)
            existsSmaller = order.lt(image, point)
          }
          el = orbit.nextInCurrentCheck(el + 1)
        }
        if (existsSmaller) {
          if (emptyMutableOrbit.nonEmpty) orbit.clear()
          return false
        }
      }
      orbit.step()
      start = orbit.nextInCurrentCheck(0)
    }
    if (emptyMutableOrbit.nonEmpty) orbit.clear()
    true
  }

  def apply[G](point: Int, generators: Iterable[G])(implicit G: PermutationAction[G]): Set[Int] =
    apply(point, generators, Opt.empty[MutableOrbit])

  def apply[G](point: Int, generators: Iterable[G], emptyMutableOrbit: Opt[MutableOrbit])
              (implicit G: PermutationAction[G]): Set[Int] =
    collection.immutable.BitSet.fromBitMaskNoCopy(orbitBitMask(point, generators, emptyMutableOrbit))

  /** Iterates through the given orbit according to the given generators. */
  def iterateOrbit[G: PermutationAction](orbit: MutableOrbit, firstStart: Int, generators: Iterable[G]): Unit = {
    val generatorArray = GenericArray[G](generators)
    var start = firstStart
    while (start != -1) {
      cforRange(0 until generatorArray.length) { gi =>
        val g = generatorArray(gi)
        var el = start
        while (el != -1) {
          val image = el <|+| g
          if (!orbit.inOrbit(image))
            orbit.addNew(image)
          el = orbit.nextInCurrentCheck(el + 1)
        }
      }
      orbit.step()
      start = orbit.nextInCurrentCheck(0)
    }
  }

  /** Returns the orbit of `point` under the elements `generators`. */
  def orbitBitMask[G: PermutationAction](point: Int, generators: Iterable[G]): Array[Long] =
    orbitBitMask(point, generators, Opt.empty[MutableOrbit])

  /** Returns the orbit of `point` under the elements `generators`. An optional
    * empty [[MutableOrbit]] can be provided; it will be cleared when the function returns.
    *
    * @param emptyMutableOrbit (Optional) Provided empty mutable orbit to avoid allocations
    */
  def orbitBitMask[G: PermutationAction](point: Int, generators: Iterable[G],
                                         emptyMutableOrbit: Opt[MutableOrbit]): Array[Long] = {
    val orbit = emptyMutableOrbit match {
      case Opt(o) => o
      case _ => MutableOrbit.empty
    }
    orbit.addNew(point)
    iterateOrbit(orbit, point, generators)
    val bm = orbit.toBitMask
    if (emptyMutableOrbit.nonEmpty) orbit.clear()
    bm
  }

  /** Returns the union of orbits of each of the `points` under the elements `generators`. */
  def orbitUnionBitMask[G: PermutationAction](points: Set[Int], generators: Iterable[G]): Array[Long] =
    orbitUnionBitMask(points, generators, Opt.empty[MutableOrbit])

  /** Returns the union of orbits of each of the `points` under the elements `generators`.
    * An optional empty [[MutableOrbit]] can be provided; it will be cleared when the function returns.
    *
    * @param emptyMutableOrbit (Optional) Provided empty mutable orbit to avoid allocations
    */
  def orbitUnionBitMask[G: PermutationAction](points: Set[Int], generators: Iterable[G],
                                              emptyMutableOrbit: Opt[MutableOrbit]): Array[Long] = {
    if (points.isEmpty) return new Array[Long](0)
    val orbit = emptyMutableOrbit match {
      case Opt(o) => o
      case _ => MutableOrbit.empty
    }
    orbit.addNew(points)
    iterateOrbit(orbit, points.min, generators)
    val bm = orbit.toBitMask
    if (emptyMutableOrbit.nonEmpty) orbit.clear()
    bm
  }

}
