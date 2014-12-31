package net.alasc.algebra

import org.scalatest.{FunSuite, NonImplicitAssertions, Matchers, EqMatchers}

import spire.algebra.NullboxSemigroupoid
import spire.syntax.semigroupoid._
import spire.util.Nullbox

import net.alasc.util._

class SemigroupoidSuite extends FunSuite with NonImplicitAssertions with Matchers {
  test("Test semigroupoid of addition of Seq[Int] with compatible sizes") {
    implicit object SeqSemigroupoid extends NullboxSemigroupoid[Seq[Int]] {
      override def opIsDefined(f: Seq[Int], g: Seq[Int]) = f.size == g.size
      def partialOp(f: Seq[Int], g: Seq[Int]): Nullbox[Seq[Int]] =
        if (f.size == g.size)
          Nullbox((f zip g).map { case (a,b) => a + b })
        else
          Nullbox.empty[Seq[Int]]
    }

    (Seq(1,2) |+|?? Seq(1,2)) shouldBe true

    (Seq(1,2,3) |+|?? Seq(1,2)) shouldBe false

    (Seq(1,2) |+|? Seq(1,2,3)).isEmpty shouldBe true

    (Seq(1,2) |+|? Seq(1,2)).get shouldBe Seq(2,4)

    (Seq(1,2) |+| Seq(1,2)) shouldBe Seq(2,4)
  }
}
