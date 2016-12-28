package net.alasc.blackbox

import scala.annotation.tailrec
import scala.reflect.ClassTag

import spire.algebra.{Eq, Group}
import spire.syntax.cfor._
import spire.syntax.eq._
import spire.syntax.group._

import metal.syntax._

object Dimino {

  /** Generates the elements of the group according to the Simple Dimino's Algorithm,
    * see page 20 of G. Butler, "Fundamental Algorithms for Permutation Groups", Springer 1991
    *
    * @param s  Array containing the group generators
    * @return   An array containing the group elements
    */
  def impl[G:ClassTag:Eq:Group](s: Array[G]): Array[G] = {
    // treat the special case <s1>
    val elements = metal.mutable.Buffer(Group[G].id)

    var g = s(0)
    while (!g.isId) {
      elements += g
      g = g |+| s(0)
    }

    @inline def order: Int = elements.length
    @tailrec @inline def contained(el: G, j: Int, n: Int): Boolean =
      if (j >= n) false
      else if (elements(j) === el) true
      else contained(el, j + 1, n)

    // treat remaining inductive levels
    cforRange(1 until s.length) { i =>
      if (!contained(s(i), 0, elements.length)) { // next generator is not redundant
         val previousOrder = order // i.e. |H_{i-1}|
        // first useful coset representative is s_i --- add a coset
        elements += s(i)
        cforRange(1 until previousOrder) { j =>
          elements += elements(j) |+| s(i)
        }

        // get coset representative's position
        var repPos = previousOrder
        do {
          cforRange(0 to i) { k =>
            val elt = elements(repPos) |+| s(k)
            if (!contained(elt, 0, elements.length)) { // add coset
              elements += elt
              cforRange(1 until previousOrder) { j =>
                elements += elements(j) |+| elt
              }
            }
          }
          repPos += previousOrder
        } while (repPos < order)
      } // not s_i in elements
    } // i = 2, ...
    elements.toArray
  }

}
