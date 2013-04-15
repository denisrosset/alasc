package com.faacets.perm

trait ExplicitPermutationGroup extends PermutationGroup {
  type Group <: ExplicitPermutationGroup
  type Element <: ExplicitPermutation

  def make(img: Vector[Domain]): Element

  abstract class ExplicitPermutation(val img: Vector[Domain]) extends Permutation {
    self: Element =>
    def compare(that: Element): Int = {
      import scala.math.Ordering.Implicits._
      Ordering[Vector[Int]].compare(img, that.img)
    }
    def images = img
    def image(el: Domain) = img(el)
    def *(other: Element): Element = {
      assert(domainSize == other.domainSize)
      make(Vector(img.map(other.img(_)):_*))
    }
    def inverse: Element = {
      val a = Array.fill[Domain](domainSize)(0)
      for (i <- 0 until domainSize) a(image(i)) = i
      make(Vector(a:_*))
    }
    def isIdentity: Boolean = (img zip (0 until domainSize)).forall( x => x._1 == x._2 )
    def identity: Element = make(Vector(0 until domainSize:_*))
    def assertValid {
      val notInside = scala.collection.mutable.BitSet((0 until domainSize): _*)
        (0 until domainSize).map(image(_)).foreach(i => {
          assert(notInside(i)) // should not be already inside, thus a duplicate element
          notInside -= i
        })
      assert(notInside.isEmpty)
    }
    override def equal(that: Element): Boolean = (img == that.img)
  }
}
