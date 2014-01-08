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

object FiniteElement {
/*
Given an element \\( e \\), and \\( n \\) elements `generators` \\( = g_1, ..., g_n \\), 
associated with a `cost` function, finds heuristically an element
\\( e' = g_k g_l ... e g_m g_n ... \\) such that \\( e' \\) has minimal cost.

The algorithm is started with `forms = Seq(e)` and `remainingIterations = iterations`, and
each iteration does the following:

- we define `m` as the minimum of `maximumForms` and
  \\( \left \lfloor \frac{\verbatim{maximumProducts}}{2 n} \right \rfloor  \\),
- we form all the products \\( g_j f \\) and \\( f g_j \\), where \\( f \\) is an element
  of `forms` and \\( g_j \\) an element of `generators`,
- from the original sequence `forms` and these new products, we keep the \\( m \\) minimal elements,
  which are used in the next iteration of the algorithm.

At the last iteration, we return the form with minimal cost.
*/
  import collection.mutable.SortedSet
  @annotation.tailrec def findMinimalOfForms[E <: FiniteElement[E]](forms: Seq[E], generators: Seq[E], cost: E => Int, m: Int, remainingIterations: Int): E = {
    implicit val orderByCost = Ordering[Int].on[E](e => cost(e))
    remainingIterations match {
      case 0 => forms.min
      case _ => {
        val set = SortedSet(forms:_*)
        for (f <- forms; g <- generators) {
          set += f * g
          set += g * f
          while (set.size > m) {
            set -= set.max
          }
        }
        findMinimalOfForms(set.toSeq, generators, cost, m, remainingIterations - 1)
      }
    }
  }

  def findMinimalForm[E <: FiniteElement[E]](e: E, generators: Seq[E], cost: E => Int, maximumProducts: Int = 10000, maximumForms: Int = 100, iterations: Int = 3): E = {
    val m: Int = math.min(maximumForms, maximumProducts / (2 * generators.size))
    findMinimalOfForms(Seq(e), generators, cost, m, iterations)
  }
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

  def toGroup(action: Action[E]): Group[E] = FGroup.fromRandomElementsAndOrder(identity, action, randomElement, order)
}
