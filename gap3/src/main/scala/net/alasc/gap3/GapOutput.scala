package net.alasc.gap3

import spire.algebra.Group
import spire.math.{Rational, SafeLong}

import cyclo.Cyclo
import fastparse.WhitespaceApi
import scalin.immutable.Vec

import net.alasc.finite.Grp
import net.alasc.perms.{Cycle, Cycles, Perm}

import net.alasc.syntax.all._
import net.alasc.perms.default._

import scalin.immutable.dense._
import scalin.syntax.all._

object GapOutput {

  val White = WhitespaceApi.Wrapper{
    import fastparse.all._
    NoTrace(CharsWhile(" \n\t".contains(_)))
  }
  import fastparse.noApi._
  import White._

  val nonNegativeInt: P[Int] = P( CharIn('0'to'9').rep(1).!.map(_.toInt) )

  val positiveInt: P[Int] = nonNegativeInt.filter(_ > 0)

  val negativeInt: P[Int] = P( "-" ~ nonNegativeInt ).map(i => -i)

  val int: P[Int] = negativeInt | nonNegativeInt

  val nonNegativeSafeLong: P[SafeLong] = P( CharIn('0'to'9').rep(1).!.map(s => SafeLong(BigInt(s))) )

  val positiveSafeLong: P[SafeLong] = nonNegativeSafeLong.filter(_ > 0)

  val negativeSafeLong: P[SafeLong] = P( "-" ~ nonNegativeSafeLong ).map(i => -i)

  val safeLong: P[SafeLong] = P("-".!.? ~ nonNegativeSafeLong).map {
    case (Some(_), sl) => -sl
    case (None, sl) => sl
  }

  def buildRational(pair: (SafeLong, Option[SafeLong])): Rational =
    Rational(pair._1, pair._2.getOrElse(SafeLong.one))

  val rational: P[Rational] = P( safeLong ~ ("/" ~ positiveSafeLong).?  ).map(buildRational)

  val nonNegativeRational: P[Rational] =
    P( nonNegativeSafeLong ~ ("/" ~ positiveSafeLong).? ).map(buildRational)

  val rootOfUnity: P[Cyclo] = P( "E" ~ "(" ~ positiveInt ~ ")" ).map(Cyclo.e(_))

  val nonNegativeCycloTerm: P[Cyclo] = P( nonNegativeRational ~ ("*" ~ rootOfUnity).? )
    .map { case (coeff, rootOpt) => Cyclo(coeff) * rootOpt.getOrElse(Cyclo.one) }

  val headCycloTerm: P[Cyclo] = P( "-".!.? ~ nonNegativeCycloTerm ).map {
    case (Some(_), ct) => -ct
    case (None, ct) => ct
  }

  val sign: P[Int] = P("+").map(unit => 1) | P("-").map(unit => -1)

  val tailCycloTerm: P[Cyclo] = P( sign ~ nonNegativeCycloTerm ).map {
    case (sign, ct) => Cyclo(sign) * ct
  }

  val cyclo: P[Cyclo] = P( headCycloTerm ~ tailCycloTerm.rep ).map {
    case (hd, tl) => tl.foldLeft(hd)(_ + _)
  }

  val dimension: P[Int] = positiveInt

  val cycle: P[Cycle] = P("(" ~ positiveInt ~ ("," ~ positiveInt).rep ~ ")").map {
    case (hd, tl) => Cycle((hd +: tl).map(_ - 1): _*)
  }

  val cycles: P[Cycles] = P(cycle.rep(1)).map(seq => Group[Cycles].combine(seq.map(cycle => cycle: Cycles)))

  val perm: P[Perm] = cycles.map(_.toPermutation[Perm])

  val permSeq: P[Seq[Perm]] = P("[" ~ perm.rep(sep =",") ~ "]")

  val cycloVec: P[Vec[Cyclo]] = P("[" ~ cyclo.rep(sep =",") ~ "]")
    .map( seq => vec(seq: _*) )

  val mon: P[Mon] = P("Mon(" ~ perm ~ "," ~ cycloVec ~ ")")
    .map { case (p, v) => Mon(p, v) }

  val groupWithGenerators: P[Grp[Perm]] = P("GroupWithGenerators" ~ "(" ~ permSeq ~ ")")
    .map( seq => Grp(seq: _*) )

  val productAMat: P[ProductAMat] = P(aMat ~ "*" ~ aMat).map { case (lhs, rhs) => ProductAMat(lhs, rhs) }

  val identityPermAMat: P[IdentityPermAMat] = P("IdentityPermAMat" ~ "(" ~ dimension ~ ")")
    .map( d => IdentityPermAMat(d) )

  val aMatPerm: P[AMatPerm] = P("AMatPerm" ~ "(" ~ perm ~ "," ~ dimension ~ ")")
    .map { case (p, d) => AMatPerm(p, d) }

  val aMatMon: P[AMatMon] = P("AMatMon" ~ "(" ~ mon ~ ")").map(AMatMon)

  val allOneAMat: P[AllOneAMat] = P("AllOneAMat" ~ "(" ~ dimension ~ ")").map(AllOneAMat)

  val nullAMat: P[NullAMat] = P("NullAMat" ~ "(" ~ dimension ~ ")").map(NullAMat)

  val diagonalAMat: P[DiagonalAMat] = P("DiagonalAMat" ~ "(" ~ cycloVec ~ ")").map(DiagonalAMat)

  val dftaMat: P[DFTAMat] = P("DFTAMat" ~ "(" ~ dimension ~ ")").map(DFTAMat)

  val soraMat: P[SORAMat] = P("SORAMat" ~ "(" ~ dimension ~ ")").map(SORAMat)

  val scalarMultipleAMat: P[ScalarMultipleAMat] = P("ScalarMultipleAMat" ~ "(" ~ cyclo ~ "," ~ aMat ~ ")")
    .map { case (s, a) => ScalarMultipleAMat(s, a) }

  val powerAMat: P[PowerAMat] = P("PowerAMat" ~ "(" ~ aMat ~ ", " ~ int ~ ")")
    .map { case (a, n) => PowerAMat(a, n) }

  val powerAMatInfix: P[PowerAMat] = P(aMat ~ "^" ~ int)
    .map { case (a, n) => PowerAMat(a, n) }

  val conjugateAMat: P[ConjugateAMat] = P("ConjugateAMat" ~ "(" ~ aMat ~ "," ~ aMat ~ ")")
    .map { case (a, b) => ConjugateAMat(a, b) }

  val directSumAMat: P[DirectSumAMat] = P("DirectSumAMat" ~ "(" ~ aMat.rep(sep = ",") ~ ")")
    .map( seq => DirectSumAMat(seq: _*) )

  val tensorProductAMat: P[TensorProductAMat] = P("TensorProductAMat" ~ "(" ~ aMat.rep(sep = ",") ~ ")")
    .map( seq => TensorProductAMat(seq: _*) )

  val galoisConjugateAMat: P[GaloisConjugateAMat] = P("GaloisConjugateAMat" ~ "(" ~ aMat ~ "," ~ int ~ ")")
    .map { case (a, k) => GaloisConjugateAMat(a, k) }

  val aMatSimple: P[AMat] = P(identityPermAMat | aMatPerm | aMatMon | allOneAMat |
    nullAMat | diagonalAMat | dftaMat | soraMat | scalarMultipleAMat | powerAMat | powerAMatInfix |
    conjugateAMat | directSumAMat | tensorProductAMat | galoisConjugateAMat)

  val aMatFactor: P[AMat] = P(aMatSimple ~ ("^" ~/ int).?).map {
    case (a, None) => a
    case (a, Some(k)) => PowerAMat(a, k)
  }

  val aMat: P[AMat] = P(aMatFactor ~ ("*" ~/ aMatFactor).rep )
    .map { case (hd, tl) => ProductAMat((hd +: tl): _*) }

}
