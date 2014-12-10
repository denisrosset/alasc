package net.alasc.algebra

import org.scalatest.{FunSuite, NonImplicitAssertions, Matchers, EqMatchers}

import net.alasc.util._
import net.alasc.syntax.semigroupoid._

class SemigroupoidSuite extends FunSuite with NonImplicitAssertions with Matchers {
  test("Test semigroupoid of addition of Seq[Int] with compatible sizes") {
    implicit object SeqSemigroupoid extends Semigroupoid[Seq[Int]] {
      def isOpDefined(f: Seq[Int], g: Seq[Int]) = f.size == g.size
      def partialOp(f: Seq[Int], g: Seq[Int]) =
        if (f.size == g.size)
          RefSome((f zip g).map { case (a,b) => a + b })
        else
          RefNone
    }

    (Seq(1,2) ?+? Seq(1,2)) shouldBe true

    (Seq(1,2,3) ?+? Seq(1,2)) shouldBe false

    (Seq(1,2) |+|? Seq(1,2,3)).isEmpty shouldBe true

    (Seq(1,2) |+|? Seq(1,2)).get shouldBe Seq(2,4)

    (Seq(1,2) |+|! Seq(1,2)) shouldBe Seq(2,4)
  }
}
