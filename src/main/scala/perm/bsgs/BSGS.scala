package com.faacets.perm
package bsgs

import scala.annotation.tailrec

abstract class BSGS[G <: PermutationGroup](val g: G) extends PermutationGroup with PermutationHasSubgroup with AbstractTransversalMixin {
  import scala.util.Random
  type Group = BSGS[G]
  type Subgroup = BSGSSubgroup
  type Element = BSGSElement

  val fullSubgroup: BSGSSubgroup
  type UnderlyingGroup = G
  override val underlyingGroup: UnderlyingGroup = g
  override type UnderlyingElement = underlyingGroup.Element

  type Base = List[Domain]
  type StrongGeneratingSet = Iterable[UnderlyingElement]

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

    def order: BigInt = next match {
      case null => transversal.size
      case _ => transversal.size * next.order
    }

    def basicSift(el: UnderlyingElement): (List[Domain], UnderlyingElement) = {
      val b = el.image(transversal.beta)
      if (!transversal.isDefinedAt(b))
        return (Nil, el)
      val nextEl = el * transversal(b)
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
      Option(next).map(_.addStrongGenerator(h))
    }
    def +=(el: UnderlyingElement) = {
      val (baseImages, h) = basicSift(el)
      def findLevel(level: BSGSConstruction, imageList: List[Domain]) {
        imageList match {
          case Nil => level.addStrongGenerator(h)
          case _ => level.next match {
            case null => if (!h.isIdentity) {
              val newBase = domain.find( h.inSupport(_) ).get
              val newTransversal = makeEmptyTransversal(newBase)
              level.next = new BSGSConstruction(newTransversal, Nil, null)
              level.next.addStrongGenerator(h)
            }
            case _ => findLevel(level.next, imageList.tail)
          }
        }
      }
      findLevel(this, baseImages)
    }
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
      BSGSSubgroupElement(transversal.randomElement._1, nextNotNullOr(next.randomElement, null))
    def generatorsIterator = sgList.iterator.map(sift(_)._1)
    def elementsIterator = for {
      b <- transversal.keysIterator
      ne <- nextNotNullOr(next.elementsIterator, List(null).iterator)
    } yield BSGSSubgroupElement(b, ne)

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
      def isIdentity = b == transversal.beta && ((nextEl eq null) || nextEl.isIdentity)
      def image(k: Domain) = transversal(b).image(nextElNotNullOr(nextEl.image(k), k))
      def images = underlying.images
      def inverse = sift(underlying.inverse)._1
      def *(that: Element) = BSGSSubgroupElement(image(that.image(transversal.beta)),
        nextElNotNullOr( {
          val n1 = nextEl.asInstanceOf[nextEl.group.Element]
          val n2 = that.nextEl.asInstanceOf[nextEl.group.Element]
          (n1 * n2).asInstanceOf[next.Element]
        }, null))
      def assertValid = ???
      def split: (UnderlyingElement, Option[bsgsSubgroup.next.Element]) = (transversal(b), Option(nextEl))
      def underlying: UnderlyingElement =
        nextElNotNullOr(transversal(b) * nextEl.underlying, transversal(b))
    }
  }
}
