package net.alasc.perms.internal

import scala.util.Random

import cats.kernel.Eq
import spire.algebra.{Group, Order}
import spire.syntax.cfor.cforRange
import net.alasc.perms.Cycle

final class PrmGroup extends Group[Prm] {
  override def isEmpty(lhs: Prm)(implicit ev: Eq[Prm]) = new PrmOps(lhs).isId
  def inverse(lhs: Prm): Prm = new PrmOps(lhs).inverse
  def empty = Prm.id
  def combine(lhs: Prm, rhs: Prm): Prm = new PrmOps(lhs) |+| rhs
}

object Prm {

  def eqv(lhs: Prm, rhs: Prm): Boolean = java.util.Arrays.equals(lhs, rhs)

  val id: Prm = (new Array[Int](0)).asInstanceOf[Prm]

  def fromImages(images: Array[Int]): Prm = {
    var k = images.length - 1
    while (k >= 0 && images(k) == k) {
      k -= 1
    }
    val array = java.util.Arrays.copyOfRange(images, 0, k + 1)
    array.asInstanceOf[Prm]
  }

  def fromImages(images: Seq[Int]): Prm = images match {
    case wa: scala.collection.mutable.WrappedArray[Int] => fromImages(wa.array)
    case _ => fromImages(images.toArray)
  }

  def fromImageFun(size: Int, imageFun: Int => Int): Prm = {
    var k = size - 1
    while (k >= 0 && imageFun(k) == k) {
      k -= 1
    }
    val array = new Array[Int](k + 1)
    cforRange(0 until k + 1) { i =>
      array(i) = imageFun(i)
    }
    array.asInstanceOf[Prm]
  }

  def fromSupportAndImageFun(support: Set[Int], imageFun: Int => Int): Prm =
    if (support.isEmpty) Prm.id else {
    val n = support.max + 1
      fromImageFun(n, imageFun)
    }

  def fromCycle(cycle: Int*): Prm = if (cycle.isEmpty) id else {
    val n = cycle.max + 1
    val array = new Array[Int](n)
    cforRange(0 until n) { i => array(i) = i }
    cforRange(0 until cycle.length - 1) { i =>
      array(cycle(i)) = cycle(i + 1)
    }
    array(cycle(cycle.length - 1)) = cycle(0)
    array.asInstanceOf[Prm]
  }

  def fromCycle(cycle: String): Prm = fromCycle(cycle.map(Cycle.alphabetMap(_)): _*)

  def fromMap(map: Map[Int, Int]): Prm =
    if (map.isEmpty) id else fromImageFun(map.keys.max + 1, i => map.getOrElse(i, i))

  def transposition(i: Int, j: Int): Prm = {
    require (i != j)
    val n = spire.math.max(i, j) + 1
    val array = new Array[Int](n)
    cforRange(0 until n) { i => array(i) = i }
    array(i) = j
    array(j) = i
    array.asInstanceOf[Prm]
  }

  def sorting[T:Order](seq: Seq[T]): Prm = {
    import spire.compat._
    fromImages(seq.zipWithIndex.sortBy(_._1).map(_._2))
  }

  def random(size: Int)(implicit gen: Random): Prm = {
    import spire.std.int._
    sorting(Seq.tabulate(size)(k => gen.nextInt))
  }

}
