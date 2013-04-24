package com.faacets.perm
package wreath
import scala.util.Random

/** Represents the base group of a inhomogenous wreath product group.
  * 
  * It is the direct product of groups A1, A2, ... An.
  */
trait InhBaseGroup extends FiniteGroup {
  type Group = InhBaseGroup
  type Element = InhBaseElement

  type A <: FiniteGroup

  val aArr: Array[AnyRef] /** Bottom group */
  val n: Int /** Number of copies */

  def a(omega: Domain): A = aArr(omega).asInstanceOf[A]
  def aSeq: Seq[A] = aArr.map(_.asInstanceOf[A])

  def assertValid { aSeq.foreach { _.assertValid } }

  override def toString = "Group representing " + aSeq.mkString(" (+) ")

  def generatorsIterator = for {
    i <- aArr.indices.iterator
    agen <- a(i).generatorsIterator
  } yield identity.updated(i, agen)

  def elementsIterator = for {
    aels <- combine((0 until n).map( i => a(i).elements )).iterator
  } yield InhBaseElement(aels.toArray)

  def contains(el: Element) = aSeq.zipWithIndex.forall { case (a, i) => a.contains(el.ael(i).asInstanceOf[a.Element]) }

  def identity = InhBaseElement(Array.tabulate[AnyRef](n)( i => a(i).identity))

  def randomElement()(implicit gen: scala.util.Random) =
    InhBaseElement(Array.tabulate(n)( i => a(i).randomElement()(gen) ))

  def order = aArr.map(_.asInstanceOf[A].order).product

  def make(aels: Seq[A#Element]) = InhBaseElement(Array.tabulate[AnyRef](n)(aels(_)))

  final case class InhBaseElement(private[wreath] val aelArr: Array[AnyRef]) extends FiniteGroupElement {
    self: Element =>
    val group = InhBaseGroup.this
    def ael(omega: Domain): A#Element = aelArr(omega).asInstanceOf[A#Element]
    def aelSeq: Seq[A#Element] = aelArr.map(_.asInstanceOf[A#Element])
    override def toString = aelSeq.mkString("(",",",")")
    def assertValid { aelSeq.foreach { _.assertValid } }
    def **[P <: PermutationGroup#PermutationElement](perm: P) =
      InhBaseElement(Array.tabulate[AnyRef](n)( i => ael(perm.inverse.image(i)) ))
    def *(that: Element) = InhBaseElement(Array.tabulate[AnyRef](n)( i => {
      val ag = a(i)
      val ael1 = aelArr(i).asInstanceOf[ag.Element]
      val ael2 = that.aelArr(i).asInstanceOf[ag.Element]
      ael1 * ael2
    } ))
    def inverse = InhBaseElement(Array.tabulate[AnyRef](n)( i => ael(i).inverse ))
    def isIdentity = aelSeq.forall( _.isIdentity )
    def equal(that: Element) = (0 until n).forall( i => {
      val ag = a(i)
      val ael1 = aelArr(i).asInstanceOf[ag.Element]
      val ael2 = that.aelArr(i).asInstanceOf[ag.Element]
      ael1 == ael2
    } )
    def updated(omega: Domain, el: A#Element) = InhBaseElement(aelArr.updated(omega, el))
  }
}
