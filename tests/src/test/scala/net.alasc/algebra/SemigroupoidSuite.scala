package net.alasc.algebra

import org.scalatest.{FunSuite, NonImplicitAssertions, Matchers}

import spire.algebra.partial.Semigroupoid
import spire.syntax.semigroupoid._
import spire.util.Opt

class SemigroupoidSuite extends FunSuite with NonImplicitAssertions with Matchers {
  test("Test semigroupoid of addition of Seq[Int] with compatible sizes") {
    implicit object SeqSemigroupoid extends Semigroupoid[Seq[Int]] {
      override def opIsDefined(f: Seq[Int], g: Seq[Int]) = f.size == g.size
      def partialOp(f: Seq[Int], g: Seq[Int]): Opt[Seq[Int]] =
        if (f.size == g.size)
          Opt((f zip g).map { case (a,b) => a + b })
        else
          Opt.empty[Seq[Int]]
    }

    (Seq(1,2) |+|?? Seq(1,2)) shouldBe true

    (Seq(1,2,3) |+|?? Seq(1,2)) shouldBe false

    (Seq(1,2) |+|? Seq(1,2,3)).isEmpty shouldBe true

    (Seq(1,2) |+|? Seq(1,2)).get shouldBe Seq(2,4)

    (Seq(1,2) |+|? Seq(1,2)).get shouldBe Seq(2,4)
  }
}
