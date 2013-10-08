package net.alasc

import scala.util.Random
import scala.reflect.ClassTag

trait FiniteElementLike {
  def isIdentity: Boolean
  def compatible(that: FiniteElementLike): Boolean
  def inverse: FiniteElementLike
  def finiteMul(that: FiniteElementLike): FiniteElementLike
}

trait FiniteElement[E <: FiniteElement[E]] extends FiniteElementLike {
  self: E =>
  def compatible(that: FiniteElementLike) = try {
    compatible(that.asInstanceOf[E])
  } catch {
    case e: ClassCastException => false
  }
  def conjugatedBy(e: E): E = (e.inverse) * this * e
  def compatible(that: E): Boolean
  def inverse: E
  def *(that: E): E
  def finiteMul(that: FiniteElementLike) = this*(that.asInstanceOf[E])
  def ===(that: E): Boolean
  override def equals(that: Any) = if (this.getClass() == that.getClass()) this === that.asInstanceOf[E] else false
}

trait FiniteGroup[E <: FiniteElement[E]] {
  /** Tests whether element e is compatible with group structure. */
  def compatible(e: E): Boolean
  /** Tests if e is contained in this group. */
  def contains(e: E): Boolean
  /** Iterates through the group elements. */
  def elements: Iterator[E]
  /** Sequence of the group generators. */
  def generators: Seq[E]
  /** Identity element of this group. */
  def identity: E
  /** Order of this group. */
  def order: BigInt

  /** Generates a random group element.
    * 
    * @param gen Instance of random generator to use.
    */
  def randomElement(gen: Random): E

  /** Generates a random group element.
    * 
    * @param gen Instance of random generator to use.
    */
  def random(implicit gen: Random = Random) = randomElement(gen)

  def elementClassTag: ClassTag[E] = ClassTag[E](identity.getClass)

  def toGroup(action: Action[E]): Group[E] = new GroupFromRandomElementsAndOrder(identity, action, randomElement, order)
}
