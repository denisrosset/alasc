package net.alasc.algebra

import spire.algebra.{Eq, Action, Group, Semigroup}
import spire.algebra.partial.Semigroupoid
import net.alasc.util._

/** Enrichs a partial algebraic structure of type `G` with a base `B` such that
  * every element `g: G` has a source and target in `B`.
  * 
  * Additional laws can then be defined, they are given in the documentation
  * of `Semigroupoid`, `PartialMonoid` and `Groupoid` TODO
  * 
  * If, in addition, this `Semigroupoid` extends `WithBase`, the following laws hold:
  * 
  *  (iv) the operation `f |+|? g` is defined if and only if `target(f) === source(g)`; then 
  * `     source(f |+| g) === source(f)` and `target(f |+| g) == target(g)`.
  * 
  * Partial monoid
  * With a base, the following laws hold:
  * 
  *   (i) source(leftId(g)) === target(leftId(g))
  *  (ii) source(rightId(g)) === target(rightId(g))
  * (iii) target(leftId(g)) === source(g)
  *  (iv) source(rightId(g)) === target(g)
  * 
  * Groupoid
  * 
  * With a base, the following laws hold:
  *   (i) target(inverse(a)) === source(a)
  *  (ii) source(inverse(a)) === target(a)
  */

trait WithBase[G, B] extends Any {
  def source(g: G): B
  def target(g: G): B
}

trait PartialMonoidWithBase[G, B] extends Any with Semigroupoid[G] with WithBase[G, B] {
  def id(b: B): G
}
