package net.alasc
package bsgs

import scala.annotation.tailrec
import scala.util.Random
import language.implicitConversions
import scala.language.higherKinds

case class SiftResult[F <: PermElement[F]](val transversalIndices: List[Dom], val remaining: F) {
  def prepend(b: Dom) = SiftResult(b :: transversalIndices, remaining)
}

sealed abstract class BSGSGroup[E <: PermElement[E]] extends PermGroup[BSGSElement[E]] with BSGSOrderings[E] with BSGSSearch[E] with BSGSMutable[E] with BSGSBase[E] with BSGSLexicographic[E] with BSGSCheck[E] {
  // Chain structure
  /** Tests whether this stabilizer chain element is the last. */
  def isTerminal: Boolean
  /** Returns the next subgroup in the stabilizer chain.
    * @note throws IllegalArgumentException on the terminal element. */
  def tail: BSGSGroup[E]
  /** Returns the length of the stabilizer chain. */
  def length: Int

  // Group operations
  def compatible(e: BSGSElement[E]): Boolean
  def contains(e: BSGSElement[E]) = true
  def contains(el: E) = basicSift(el).remaining.isIdentity
  def degree = representedIdentity.size
  def elements: Iterator[BSGSElement[E]]
  def fromExplicit(p: Perm): Option[BSGSElement[E]] = {
    implicit def conversion(e: E) = e.explicit
    val SiftResult(sequence, remaining) = basicSift(p)
    if (remaining.isIdentity)
      Some(fromSequence(sequence))
    else
      None
  }
  def generators = strongGeneratingSet.iterator.map(sift(_)._1)
  def identity: BSGSElement[E]
  def order: BigInt
  def randomElement(gen: Random): BSGSElement[E]
//  def toTeX = TeX("{}^{"+degree+"}_{"+order+"} \\text{BSGS} \\left ( \\begin{array}{" + "c"*length + "}" + base.mkString(" & ") + "\\\\" + transversalSizes.mkString(" & ") + "\\end{array} \\right )")

  // BSGS data
  /** Base element for current node in stabilizer chain. */
  def beta: Dom
  /** Base of transversal chain. */
  def base: Base
  def strongGeneratingSet: List[E]
  def representedIdentity: E
  def transversal: TransLike[E]
  /** Size of transversal of this node in stabilizer chain. */
  def transversalSize = transversal.size
  /** List of transversal sizes. */
  def transversalSizes: List[Int]

  // BSGS operations
  /** Sifts an element through the stabilizer chain. */
  def sift(e: E): (BSGSElement[E], E) = {
    assert(isImmutable)
    val SiftResult(sequence, remaining) = basicSift(e)
    (fromSequence(sequence), remaining)
  }

  /** Constructs a BSGS element from a sequence of transversal indices.
    * 
    * @param sequence Sequence of transversal indices.
    * 
    * @return Constructed BSGS group element.
    */
  def fromSequence(sequence: List[Dom]): BSGSElement[E]

  /** Constructs a BSGS element from a base image. */
  def fromBaseImage(baseImage: List[Dom]): BSGSElement[E]

  /** Sifts element f through the stabilizer chains.
    * 
    * @param f    Element to sift through
    * 
    * @return Transversal indices and the part remaining after sifting.
    */
  def basicSift[F <: PermElement[F]](f: F)(implicit conv: E => F): SiftResult[F]
  def transversalElement(level: Int, b: Dom): BSGSElement[E]

  // Modifications
  def convertElements[F <: PermElement[F]](f: E => F): BSGSGroup[F]

}

final case class BSGSGroupTerminal[E <: PermElement[E]] private[bsgs](val id: E) extends BSGSGroup[E] {
  // Mutable/immutable toggle
  private[bsgs] def isImmutable = true
  private[bsgs] def makeImmutable { }

  // Chain structure
  def isTerminal = true
  def tail = throw new IllegalArgumentException("Cannot get tail of BSGS chain terminal.")
  def length = 0
  private[this] def element = BSGSElementTerminal(this)

  // Group operations
  def compatible(e: BSGSElement[E]) = e.isTerminal
  def elements = Iterator(element)
  def identity = element
  def order = BigInt(1)
  def randomElement(gen: Random) = element

  // BSGS data
  def beta = throw new IllegalArgumentException("Cannot get base element of BSGS chain terminal.")
  def base = Base(Nil)
  def strongGeneratingSet = List.empty[E]
  def representedIdentity = id
  def transversal = throw new IllegalArgumentException("Cannot get transversal of BSGS chain terminal.")
  def transversalSizes = Nil

  // BSGS operations
  def fromSequence(sequence: List[Dom]) = { require_(sequence.isEmpty); element }
  def fromBaseImage(baseImage: List[Dom]) =  { require_(baseImage.isEmpty); element }
  def basicSift[F <: PermElement[F]](f: F)(implicit conv: E => F) = SiftResult(Nil, f)
  def transversalElement(level: Int, b: Dom) = element

  // Modifications
  def convertElements[F <: PermElement[F]](f: E => F) =
    BSGSGroupTerminal(f(id))
}

final class BSGSGroupNode[E <: PermElement[E]](
  private[bsgs] var trv: TransLike[E],
  private[bsgs] var sg: List[E],
  private[bsgs] var id: E,
  private[bsgs] var isImmutable: Boolean,
  private[bsgs] var tl: BSGSGroup[E])
    extends BSGSGroup[E] {
  // Mutable/immutable toggle
  def this(newTrv: TransLike[E], newSg: List[E], newId: E, newTl: BSGSGroup[E]) =
    this(newTrv, newSg, newId, true, newTl)
  private[bsgs] def makeImmutable {
    isImmutable = true
    tail.makeImmutable
  }

  // Chain structure
  def isTerminal = false
  def tail = tl
  def length = tail.length + 1

  // Group operations
  def compatible(e: BSGSElement[E]) = e match {
    case BSGSElementNode(g, b, tl) => g == this && tail.compatible(e.tail)
    case _ => false
  }
  def elements = for ( n <- tail.elements; b <- trv.keysIterator ) yield
    BSGSElementNode(this, b, n)
  def identity = BSGSElementNode(this, trv.beta, tail.identity)
  def order = transversalSize * tail.order
  def randomElement(gen: Random) =
    BSGSElementNode(this, trv.random(gen)._1, tail.random(gen))


  // BSGS data
  def beta = trv.beta
  def base = Base(beta :: tail.base.list)
  def strongGeneratingSet = sg
  def representedIdentity = id
  def transversal = trv
  def transversalSizes = trv.size :: tail.transversalSizes

  // BSGS operations
  def fromSequence(sequence: List[Dom]): BSGSElement[E] =
    BSGSElementNode(this, sequence.head, tail.fromSequence(sequence.tail))
  def fromBaseImage(baseImage: List[Dom]) =
    BSGSElementNode(this, baseImage.head, tail.fromBaseImage(baseImage.tail.map(k => trv.uinv(baseImage.head).image(k))))

  def basicSift[F <: PermElement[F]](f: F)(implicit conv: E => F) = {
    val b = f.image(beta)
    if (!trv.isDefinedAt(b))
      SiftResult(Nil, f)
    else {
      val nextF = f * trv.uinv(b)
      val SiftResult(transversalIndices, remaining) = tail.basicSift(nextF)
      SiftResult(b :: transversalIndices, remaining)
    }
  }
  def transversalElement(level: Int, b: Dom) = new BSGSElementNode(this, if (level == 0) b else beta, tail.transversalElement(level - 1, b))

  // Modifications
  def convertElements[F <: PermElement[F]](f: E => F) =
    new BSGSGroupNode(trv.mapValues(f), sg.map(f), f(id), tail.convertElements(f))
}
