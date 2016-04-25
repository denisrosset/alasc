package net.alasc.gap3

import spire.algebra.Group

import fastparse.WhitespaceApi

import net.alasc.finite.Grp
import net.alasc.perms.{Cycle, Cycles, Perm}

object GapOutput {

  val White = WhitespaceApi.Wrapper{
    import fastparse.all._
    NoTrace(" ".rep)
  }
  import fastparse.noApi._
  import White._

  val number: P[Int] = P( CharIn('0'to'9').rep(1).!.map(_.toInt) )

  val cycle: P[Cycle] = P("(" ~ number ~ ("," ~ number).rep ~ ")").map {
    case (hd, tl) => Cycle((hd +: tl).map(_ - 1): _*)
  }

  val cycles: P[Cycles] = P(cycle.rep(1)).map(seq => Group[Cycles].combine(seq.map(cycle => cycle: Cycles)))

  val emptySeq: P[Seq[Nothing]] = P("[" ~ "]").map(unit => Seq.empty)

  def nonEmptySeq[A](A: P[A]): P[Seq[A]] = P("[" ~ A ~ ("," ~ A).rep ~ "]").map {
    case (hd, tl) => hd +: tl
  }

  val cyclesSeq: P[Seq[Cycles]] = (emptySeq: P[Seq[Cycles]]) | nonEmptySeq(cycles)

  import net.alasc.syntax.all._
  import net.alasc.perms.default._

  val groupWithGenerators: P[Grp[Perm]] = P("GroupWithGenerators" ~ "(" ~ cyclesSeq ~ ")").map { seq => Grp(seq.map(_.toPermutation[Perm]): _*) }

}
