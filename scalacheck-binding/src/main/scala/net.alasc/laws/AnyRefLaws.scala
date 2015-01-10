package net.alasc.laws

/*import spire.algebra._
 import spire.algebra.lattice._
 import spire.implicits._
 */

import org.typelevel.discipline.Laws

import org.scalacheck.{Arbitrary, Gen, Prop}
import org.scalacheck.Prop._

import spire.algebra.Eq
import spire.std.int._
import spire.std.boolean._
import spire.syntax.eq._
import spire.syntax.bool._

case class Instances[A](seq: Seq[A]) {
  require(seq.size >= 2)
  def first: A = seq(0)
  def second: A = seq(1)
  def n: Int = seq.size
  def ++(rhs: Instances[A]): Instances[A] = Instances(seq ++ rhs.seq)
  def :+(rhs: A) = Instances(seq :+ rhs)
  def map[B](f: A => B): Instances[B] = Instances(seq.map(f))
  def flatMap[B](f: A => Instances[B]) = seq.flatMap(f(_).seq)
  def allDistinctPairs: Iterator[(A, A)] = for {
    i <- (0 until n).iterator
    j <- (i+1 until n).iterator
  } yield (seq(i), seq(j))
}

object Instances {
  def apply[A](implicit ev: Instances[A]): Instances[A] = ev
  implicit val IntInstances: Instances[Int] = Instances(Seq(-1, 0, 1))
}

case class Cloner[A](make: A => A)

object Cloner {
  def apply[A](implicit ev: Cloner[A]): Cloner[A] = ev
  implicit val IntCloner: Cloner[Int] = Cloner((x: Int) => x)
}

object AnyRefLaws {
  def apply[A:Instances:Cloner](implicit ev: Arbitrary[A]) = new AnyRefLaws[A] {
    def RndArb = ev
    def Clnr = Cloner[A]
    def Inst = Instances[A]
  }
}

trait AnyRefLaws[A] extends Laws {

  def RndArb: Arbitrary[A]
  implicit def Clnr: Cloner[A]
  implicit def Inst: Instances[A]

  implicit def Arb: Arbitrary[A] =
    Arbitrary(Gen.oneOf(Gen.oneOf(Inst.seq), RndArb.arbitrary))

  def _equals = new AnyRefProperties(
    "equals",
    parents = Nil,

    "Clone equals original" → forAll((a: A) =>
      Clnr.make(a) == a
    ),

    "All instances are different" →
      Prop(Inst.allDistinctPairs.forall { case (x, y) => x != y })
  )

  def _hashCode = new AnyRefProperties(
    "hash",
    parents = Nil,

    "At least two instances have different hashCodes" →
      Prop(Inst.seq.map(_.hashCode).toSet.size > 1),

    "clone has same hashCode" → forAll((a: A) =>
      Clnr.make(a).hashCode === a.hashCode
    ),

    "(x == y) => (x.hashCode == y.hashCode)" → forAll((x: A, y: A) =>
      (x == y) imp (x.hashCode == y.hashCode)
    )
  )

  def _anyRef = new AnyRefProperties(
    "anyRef",
    parents = Seq(_hashCode, _equals)
  )

  def _eq(implicit Equ: Eq[A]) = new AnyRefProperties(
    "eq",
    parents = Seq(_anyRef),

    "equals compatible with Eq" → forAll((x: A, y: A) =>
      (x === y) == (x == y)
    )
  )

  class AnyRefProperties(
    val name: String,
    val parents: Seq[AnyRefProperties],
    val props: (String, Prop)*
  ) extends RuleSet {
    def bases = Seq.empty[(String, Laws#RuleSet)]
  }
}
