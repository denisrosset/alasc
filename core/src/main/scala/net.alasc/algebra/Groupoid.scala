package net.alasc.algebra

import spire.algebra.{Eq, Action, Group, Semigroup}
import net.alasc.util._

/** A semigroupoid is a set with a partial binary operation `partialOp`, which is
  * associative in the following sense: if f,g,h are elements of the semigroupoid such
  * that either:
  *   (i) f |+|? g is defined and g |+|? h is defined
  *  (ii) f |+|? g is defined and (f |+| g) |+|? h is defined
  * (iii) g |+|? h is defined and f |+|? (g |+| h) is defined
  * 
  * then all of f |+|? g, g |+|? h, (f |+|? g) |+|? h, f |+|? (g |+|? h)
  * are defined and (f |+| g) |+| h = f |+| (g |+| h).
  * 
  * If, in addition, this `Semigroupoid` extends `WithBase`, the following laws hold:
  * 
  *  (iv) the operation `f |+|? g` is defined if and only if `target(f) === source(g)`; then 
  * `     source(f |+| g) === source(f)` and `target(f |+| g) == target(g)`.
  */
trait Semigroupoid[G] extends Any {
  def isOpDefined(f: G, g: G): Boolean
  def partialOp(f: G, g: G): Option[G]
  def forceOp(f: G, g: G): G = partialOp(f, g) match {
    case Some(result) => result
    case _ => throw new IllegalArgumentException(s"$f |+|! $g is not defined")
  }

  implicit def OptionSemigroup: Semigroup[Option[G]] = new Semigroup[Option[G]] {
    def op(fOpt: Option[G], gOpt: Option[G]) = fOpt match {
      case Some(f) => gOpt match {
        case Some(g) => partialOp(f, g)
        case None => None
      }
      case None => None
    }
  }
}

/** Enrichs a partial algebraic structure of type `G` with a base `B` such that
  * every element `g: G` has a source and target in `B`.
  * 
  * Additional laws can then be defined, they are given in the documentation
  * of `Semigroupoid`, `PartialMonoid` and `Groupoid`.
  */
trait WithBase[G, B] extends Any {
  def source(g: G): B
  def target(g: G): B
}

/** A partial monoid is a set with a partial binary operation where left and right identity elements
  * are defined for every element, such that:
  * 
  *   (i) (leftId(g) |+|? g).get === g
  *  (ii) (g |+|? rightId(g)).get === g
  * 
  * With a base, the following laws hold:
  * 
  *   (i) source(leftId(g)) === target(leftId(g))
  *  (ii) source(rightId(g)) === target(rightId(g))
  * (iii) target(leftId(g)) === source(g)
  *  (iv) source(rightId(g)) === target(g)
  */
trait PartialMonoid[G] extends Any with Semigroupoid[G] {
  def isId(g: G): Boolean
  def leftId(g: G): G
  def rightId(g: G): G
}

trait PartialMonoidWithBase[G, B] extends Any with Semigroupoid[G] with WithBase[G, B] {
  def id(b: B): G
}

/** A groupoid is a partial monoid, where every element has an inverse.
  *
  *   (i) `inverse(a) |+|? a` and `a |+|? inverse(a)` are always defined
  *  (ii) if `a |+|? b` is defined, then `a |+|? b |+|? inverse(b) === a` and `inverse(a) |+|? a |+|? b === b`
  * 
  * With a base, the following laws hold:
  *   (i) target(inverse(a)) === source(a)
  *  (ii) source(inverse(a)) === target(a)
  */
trait Groupoid[G] extends Any with PartialMonoid[G] {
  def inverse(g: G): G
  def partialOpInverse(f: G, g: G): Option[G] = partialOp(f, inverse(g))
  def forceOpInverse(f: G, g: G): G = forceOp(f, inverse(g))
  def isOpInverseDefined(f: G, g: G): Boolean = isOpDefined(f, inverse(g))
  def leftId(g: G): G = forceOp(g, inverse(g))
  def rightId(g: G): G = forceOp(inverse(g), g)
}
