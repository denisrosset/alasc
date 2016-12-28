package net.alasc.laws

import org.scalacheck.{Arbitrary, Gen}

case class Dom(val value: Int) extends AnyVal

object Dom {

  val genDom: Gen[Dom] = Gen.oneOf(
    Gen.choose(0, 20),
    Gen.choose(0, 100),
    Gen.choose(0, 100000)
  ).map(Dom(_))

  implicit val arbDom: Arbitrary[Dom] = Arbitrary(genDom)

  implicit def dom2int(d: Dom): Int = d.value

}
