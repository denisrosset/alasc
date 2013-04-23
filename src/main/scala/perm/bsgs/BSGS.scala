package com.faacets.perm
package bsgs

import scala.annotation.tailrec
import scala.util.Random

trait BSGSTypes {
  type UnderlyingGroup <: PermutationGroup
  val underlyingGroup: UnderlyingGroup
  type UnderlyingElement = underlyingGroup.Element

  type Transversal <: AbstractTransversal
  def makeEmptyTransversal(beta: Domain): Transversal
  /** Trait for transversals of the stabilizers chain.
    * 
    * A transversal is defined with respect to an element beta, and for every element
    * b in the orbit of beta returns a permutation u_b such that:
    * beta ^ u_b = b.
    * 
    * We also store the permutation uinv_b = u_b^-1 such that
    * b ^ uinv_b = beta.
    * 
    * When called, the transversal defined below returns a pair (u_b, uinv_b).
    */
  trait AbstractTransversal extends PartialFunction[Domain, (UnderlyingElement, UnderlyingElement)] with Iterable[(Domain, (UnderlyingElement, UnderlyingElement))] {
    type ElementPair = (UnderlyingElement, UnderlyingElement)
    def beta: Domain /** Element for which the transversal is defined. */
    def u(b: Domain) = apply(b)._1
    def uinv(b: Domain) = apply(b)._2
    // we do not inherit from Map[Domain, UnderlyingElement], but define
    // *some* of its methods
    def keysIterator: Iterator[Domain] = iterator.map(_._1)
    def valuesIterator: Iterator[ElementPair] = iterator.map(_._2)

    /** Returns a random element of the transversal. */
    def randomElement()(implicit gen: Random = Random): (Domain, ElementPair) = {
      val num = gen.nextInt(size)
      iterator.drop(num).next()
    }
    def addingGenerator(s: UnderlyingElement): Transversal  /** Returns a new transversal extended with s added to its generators. */
      /** Checks the sanity of the transversal. */
    def assertValid {
      for ((b, (ub, uinvb)) <- iterator)
        assert(ub.image(beta) == b && uinvb.image(b) == beta)
    }
  }
}

trait BSGS extends PermutationGroup with PermutationHasSubgroup with BSGSTypes {
  type Group = BSGS
  type Subgroup = BSGSSubgroup
  type Element = BSGSElement

  val fullSubgroup: BSGSSubgroup = null
  type UnderlyingGroup <: PermutationGroup
  override val underlyingGroup: UnderlyingGroup
  override type UnderlyingElement = underlyingGroup.Element

  type Base = List[Domain]
  type StrongGeneratingSet = Iterable[UnderlyingElement]

  def assertValid = ???
  def identity = BSGSElement(fullSubgroup.identity)
  def randomElement()(implicit gen: Random = Random) = BSGSElement(fullSubgroup.randomElement()(gen))
  def contains(el: Element) = true
  def order = fullSubgroup.order
  def elementsIterator = fullSubgroup.elementsIterator.map(BSGSElement(_))
  def generatorsIterator = fullSubgroup.generatorsIterator.map(BSGSElement(_))
  def degree = underlyingGroup.degree

  final case class BSGSElement(el: fullSubgroup.Element) extends PermutationElement {
    self: Element =>
    val group = BSGS.this
    def *(that: Element) = BSGSElement(el * that.el)
    def assertValid = el.assertValid
    def equal(that: Element) = el == that.el
    def inverse = BSGSElement(el.inverse)
    def isIdentity = el.isIdentity
    def image(k: Domain) = el.image(k)
    def images = el.images
    def underlying = el.underlying
  }

  private[bsgs] trait BSGSBasics {
    def transversal: Transversal
    private[bsgs] def sgList: List[UnderlyingElement]
    private[bsgs] def next: BSGSBasics
    def nextNotNullOr[Result](f: => Result, inCaseOfNull: Result): Result = next match {
      case null => inCaseOfNull
      case _ => f
    }

    @tailrec final def length(acc: Int = 1): Int = next match {
      case null => acc
      case _ => next.length(acc + 1)
    }

    def contains(el: UnderlyingElement) = basicSift(el)._2.isIdentity
    def uSize: List[Int] = nextNotNullOr(transversal.size :: next.uSize, transversal.size :: Nil)
    def order: BigInt = nextNotNullOr(transversal.size * next.order, transversal.size)

    def basicSift(el: UnderlyingElement): (List[Domain], UnderlyingElement) = {
      val b = el.image(transversal.beta)
      if (!transversal.isDefinedAt(b))
        return (Nil, el)
      val nextEl = el * transversal.uinv(b)
      next match {
        case null => (b :: Nil, el)
        case _ => {
          val (bList, retEl) = next.basicSift(nextEl)
          (b :: bList, retEl)
        }
      }
    }
  }
  private[bsgs] final case class BSGSConstruction(
    var transversal: Transversal,
    private[bsgs] var sgList: List[UnderlyingElement],
    private[bsgs] var next: BSGSConstruction) extends BSGSBasics {

    def addStrongGenerator(h: UnderlyingElement) {
      sgList = h :: sgList
      transversal = transversal.addingGenerator(h)
    }
    def construct(el: UnderlyingElement): Option[UnderlyingElement] = {
      val b = el.image(transversal.beta)
      if (!transversal.isDefinedAt(b)) {
        addStrongGenerator(el)
        return Some(el)
      }
      val h = el * transversal.uinv(b)
      if (next eq null) {
        if (h.isIdentity)
          return None
        val newBase = domain.find( h.inSupport(_) ).get
        val newTransversal = makeEmptyTransversal(newBase)
        next = new BSGSConstruction(newTransversal, Nil, null)
        next.addStrongGenerator(h)
        addStrongGenerator(h)
        return Some(h)
      } else {
        next.construct(h) match {
          case None => return None
          case Some(gen) => {
            addStrongGenerator(gen)
            return Some(gen)
          }
        }
      }
    }

    def +=(el: UnderlyingElement) = construct(el)
    def asSubgroup: BSGSSubgroup = BSGSSubgroup(transversal, sgList, nextNotNullOr(next.asSubgroup, null))
  }

  private[bsgs] object BSGSConstruction {
    def fromBase(base: Base): BSGSConstruction = {
      def create(levelBase: Base): BSGSConstruction = levelBase match {
        case Nil => null
        case hd :: tl => BSGSConstruction(makeEmptyTransversal(hd), Nil, create(tl))
      }
      if (base.isEmpty)
        create(List(0))
      else
        create(base)
    }
  }

  final case class BSGSSubgroup(
    val transversal: Transversal,
    private[bsgs] val sgList: List[UnderlyingElement],
    private[bsgs] val next: BSGSSubgroup) extends BSGSBasics with PermutationSubgroup {
    bsgsSubgroup: Subgroup =>
    type Group = BSGSSubgroup
    type Element = BSGSSubgroupElement
    val mySuperGroup = BSGS.this
    def nextSubgroup = Option(next)
    def make(baseImages: List[Domain]): Element =
      BSGSSubgroupElement(baseImages.head, nextNotNullOr(next.make(baseImages.tail), null))
    def sift(el: UnderlyingElement): (Element, UnderlyingElement) = {
      val (baseImages, remaining) = basicSift(el)
      (make(baseImages), remaining)
    }
    def contains(el: Element) = true
    def assertValid = ???
    def identity: Element = BSGSSubgroupElement(transversal.beta, nextNotNullOr(next.identity, null))
    def randomElement()(implicit gen: scala.util.Random) =
      BSGSSubgroupElement(transversal.randomElement()(gen)._1, nextNotNullOr(next.randomElement()(gen), null))
    def generatorsIterator = sgList.iterator.map(sift(_)._1)
    def elementsIterator = for {
      b <- transversal.keysIterator
      ne <- nextNotNullOr(next.elementsIterator, List(null).iterator)
    } yield BSGSSubgroupElement(b, ne)

    /** Describes a subgroup element by its base image.
      * 
      * The subgroup element is written as g = u_m-1 * ... * u1 * u0
      */
    final case class BSGSSubgroupElement(val b: Domain, private[bsgs] val nextEl: bsgsSubgroup.next.Element) extends PermutationSubgroupElement {
      self: bsgsSubgroup.Element =>
      val group = BSGSSubgroup.this
      def nextElNotNullOr[R](f: => R, v: R) = nextEl match {
        case null => v
        case _ => f
      }
      def equal(that: Element): Boolean = (b == that.b) && nextElNotNullOr( {
        val n1 = nextEl.asInstanceOf[nextEl.group.Element]
        val n2 = that.nextEl.asInstanceOf[nextEl.group.Element]
        n1.equal(n2)
      }, true)
      def superElement = BSGSElement(fullSubgroup.sift(underlying)._1)
      def isIdentity = (b == transversal.beta) && ((nextEl eq null) || nextEl.isIdentity)
      // The element is written as g = u_m-1 * ... * u1 * u0,
      // and the action on the domain is a left action, so we return
      // k^g = k^(u_m-1 ... * u1)^u0 applying first the next levels
      def image(k: Domain) = transversal.u(b).image(nextElNotNullOr(nextEl.image(k), k))
      def images = underlying.images
      def inverse = sift(underlying.inverse)._1
      // We have this = g = u_m-1 * ... * u1 * u0 and that = h = v_m-1 * ... * v1 * v0,
      // the base image beta^g is given by beta^(g*h) = beta^(u_m-1 * ... * u1 * u0 * h) =
      // = beta^(u0 * h) = b^h because the elements u_m-1 * ... * u1 do not move beta
      def *(that: Element) = BSGSSubgroupElement(that.image(b),
        nextElNotNullOr( {
          val n1 = nextEl.asInstanceOf[nextEl.group.Element]
          val n2 = that.nextEl.asInstanceOf[nextEl.group.Element]
          (n1 * n2).asInstanceOf[next.Element]
        }, null))
      def assertValid = ???
      def split: (Option[bsgsSubgroup.next.Element], UnderlyingElement) = (Option(nextEl), transversal.u(b))
      // The element is written as g = u_m-1 * ... * u1 * u0,
      // so we must fold the chain from left to right
      def underlying: UnderlyingElement =
        nextElNotNullOr(nextEl.underlying * transversal.u(b), transversal.u(b))
    }
  }
}
