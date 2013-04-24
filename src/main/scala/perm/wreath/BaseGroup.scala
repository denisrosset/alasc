package com.faacets.perm
package wreath
import scala.util.Random

/** Represents the base group of a wreath product group.
  * 
  * It is the direct product of a group G n times with itself.
  */
trait BaseGroup extends FiniteGroup {
  type Group = BaseGroup
  type Element = BaseElement

  type A <: FiniteGroup

  val a: A /** Bottom group */
  val n: Int /** Number of copies */

  def assertValid { a.assertValid }
  override def toString = "Group representing " + n + " copies of " + a
  def generatorsIterator = for {
    agen <- a.generatorsIterator
    omega <- (0 until n)
    } yield identity.updated(omega, agen)

  def elementsIterator = for {
    aels <- combine((0 until n).map(i => a.elements)).iterator // loop over iterators on the each copy of a
  } yield BaseElement(aels.toArray)

  def contains(el: Element) = (0 until n).forall( i => a.contains(el.ael(i)) )

  def identity = BaseElement(Array.fill[AnyRef](n)(a.identity))
  def randomElement()(implicit gen: scala.util.Random) =
    BaseElement(Array.tabulate(n)( i => a.randomElement()(gen) ))
  def order = a.order.pow(n)
  def make(aels: Seq[a.Element]) = BaseElement(Array.tabulate[AnyRef](n)(aels(_)))
  final case class BaseElement(private[wreath] val arr: Array[AnyRef]) extends FiniteGroupElement {
    self: Element =>
    val group = BaseGroup.this
    override def toString = (0 until n).map(ael(_).toString).mkString("(",",",")")
    def assertValid { (0 until n).foreach { ael(_).assertValid } }
    def **[P <: PermutationGroup#PermutationElement](perm: P) =
      BaseElement(Array.tabulate[AnyRef](n)( i => ael(perm.inverse.image(i)) ))
    def ael(omega: Domain) = arr(omega).asInstanceOf[a.Element]
    def *(that: Element) = BaseElement(Array.tabulate[AnyRef](n)( i => ael(i)*that.ael(i)))
    def inverse = BaseElement(Array.tabulate[AnyRef](n)( i => ael(i).inverse ))
    def isIdentity = (0 until n).forall( ael(_).isIdentity )
    def equal(that: Element) = (0 until n).forall( i => ael(i).equal(that.ael(i)) )
    def updated(omega: Domain, el: a.Element) = BaseElement(arr.updated(omega, el))
  }
}
