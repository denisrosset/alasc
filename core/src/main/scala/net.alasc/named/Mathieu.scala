package net.alasc.named

import net.alasc.algebra._
import net.alasc.finite.{Grp, GrpBuilder}
import net.alasc.perms._

object Mathieu {

  import NamedGroups.generate

  val generatorsAndOrders: Map[Int, (Iterable[Perm], BigInt)] =
    Map(
      10 -> ((Seq(Perm(1,2)(3,4), Perm(1,2,3,5)(4,6)), 360)),
      11 -> ((Seq(Perm(1,2,3,4,5,6,7,8,9,10,11), Perm(3,7,11,8)(4,10,5,6)), 7920)),
      12 -> ((Seq(Perm(1,2,3,4,5,6,7,8,9,10,11), Perm(1,12)(2,11)(3,6)(4,8)(5,9)(7,10), Perm(3,7,11,8)(4,10,5,6)), 95040)),
      20 -> ((
        Seq(Perm(1,2,4,3)(5,11,7,12)(6,13)(8,14)(9,15,10,16)(17,19,20,18),
          Perm(2,5,6)(3,7,8)(4,9,10)(11,17,12)(13,16,18)(14,15,19)),
        960
      )),
      21 -> ((
        Seq(Perm(1,2)(4,6)(5,7)(8,12)(9,14)(10,15)(11,17)(13,19),
          Perm(2,3,5,4)(6,8,13,9)(7,10,16,11)(12,18)(14,20,21,15)(17,19)),
        20160
      )),
      22 -> ((
        Seq(Perm(1,13)(2,8)(3,16)(4,12)(6,22)(7,17)(9,10)(11,14),
          Perm(1,22,3,21)(2,18,4,13)(5,12)(6,11,7,15)(8,14,20,10)(17,19)),
        443520
      )),
      23 -> ((
        Seq(Perm(1,2)(3,4)(7,8)(9,10)(13,14)(15,16)(19,20)(21,22),
          Perm(1,16,11,3)(2,9,21,12)(4,5,8,23)(6,22,14,18)(13,20)(15,17)),
        BigInt(10200960)
      )),
      24 -> ((Seq(Perm("0123456789ABCDEFGHIJKLM")("N"),
        Perm("0N")("1M")("2B")("3F")("4H")("59")("6J")("7D")("8K")("AG")("CL")("EI"),
        Perm("2G968")("3CDI4")("7HABM")("EJLKF")),
        BigInt(244823040)
      ))
    )

  def notDefined: Nothing = throw new IllegalArgumentException("Mathieu groups are only defined for degrees " + generatorsAndOrders.keys.toSeq.sorted.mkString(", "))

  def apply[G:PermutationBuilder:GrpBuilder](degree: Int): Grp[G] = {
    val (generators, order) = generatorsAndOrders.getOrElse(degree, notDefined)
    generate[G](generators, order)
  }

}
