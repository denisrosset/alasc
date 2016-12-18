package net.alasc.gap3

import spire.algebra.Group
import spire.math.{Rational, SafeLong}

import cyclo.Cyclo
import fastparse.WhitespaceApi
import scalin.Subscript
import scalin.immutable.{Mat, Vec}

import net.alasc.finite.{Grp, GrpGroup}
import net.alasc.perms.{Cycle, Cycles, Perm}
import net.alasc.syntax.all._
import net.alasc.perms.default._
import scalin.immutable.dense._
import scalin.syntax.all._
/*
object GapOutput {

  val White = WhitespaceApi.Wrapper{
    import fastparse.all._
    NoTrace(CharsWhile(" \n\t".contains(_)).?)
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

  val rootOfUnityPower: P[Cyclo] = P( rootOfUnity ~ ("^" ~ int).? ).map {
    case (root, optExp) => root.pow(optExp.getOrElse(1))
  }

  val nonNegativeCoeffCycloTerm: P[Cyclo] = P( nonNegativeRational ~ ("*" ~ rootOfUnityPower).? )
    .map { case (coeff, rootOpt) => Cyclo(coeff) * rootOpt.getOrElse(Cyclo.one) }

  val nonNegativeCycloTerm: P[Cyclo] = rootOfUnityPower | nonNegativeCoeffCycloTerm

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

  val permId: P[Perm] = P("(" ~ ")").map( unit => Perm.id )

  val perm: P[Perm] = permId | cycles.map(_.toPermutation[Perm])

  val permSeq: P[Seq[Perm]] = P("[" ~ perm.rep(sep =",") ~ "]")

  val cycloVec: P[Vec[Cyclo]] = P("[" ~ cyclo.rep(sep =",") ~ "]")
    .map( seq => vec(seq: _*) )

  val dimensionAsVec: P[Vec[Cyclo]] = positiveInt.map( d => ones[Cyclo](d) )
  val mon: P[Mon] = P("Mon(" ~ ( perm ~ ",").? ~ (dimensionAsVec | cycloVec) ~ ")")
    .map { case (pOpt, v) => Mon(pOpt.getOrElse(Perm.id), v) }

  val productAMat: P[ProductAMat] = P(aMat ~ "*" ~ aMat).map { case (lhs, rhs) => ProductAMat(lhs, rhs) }

  val identityPermAMat: P[IdentityPermAMat] = P("IdentityPermAMat" ~/ "(" ~ dimension ~ ")")
    .map( d => IdentityPermAMat(d) )

  val aMatPerm: P[AMatPerm] = P("AMatPerm" ~/ "(" ~ perm ~ "," ~ dimension ~ ")")
    .map { case (p, d) => AMatPerm(p, d) }

  val aMatMon: P[AMatMon] = P("AMatMon" ~/ "(" ~ mon ~ ")").map(AMatMon)

  val allOneAMat: P[AllOneAMat] = P("AllOneAMat" ~/ "(" ~ dimension ~ ")").map(AllOneAMat)

  val nullAMat: P[NullAMat] = P("NullAMat" ~/ "(" ~ dimension ~ ")").map(NullAMat)

  val diagonalAMat: P[DiagonalAMat] = P("DiagonalAMat" ~/ "(" ~ cycloVec ~ ")").map(DiagonalAMat)

  val dftaMat: P[DFTAMat] = P("DFTAMat" ~/ "(" ~ dimension ~ ")").map(DFTAMat)

  val soraMat: P[SORAMat] = P("SORAMat" ~/ "(" ~ dimension ~ ")").map(SORAMat)

  val scalarMultipleAMat: P[ScalarMultipleAMat] = P("ScalarMultipleAMat" ~ "(" ~ cyclo ~ "," ~ aMat ~ ")")
    .map { case (s, a) => ScalarMultipleAMat(s, a) }

  val powerAMat: P[PowerAMat] = P("PowerAMat" ~/ "(" ~ aMat ~ ", " ~ int ~ ")")
    .map { case (a, n) => PowerAMat(a, n) }

  val powerAMatInfix: P[PowerAMat] = P(aMat ~ "^" ~ int)
    .map { case (a, n) => PowerAMat(a, n) }

  val conjugateAMat: P[ConjugateAMat] = P("ConjugateAMat" ~/ "(" ~ aMat ~ "," ~ aMat ~ ")")
    .map { case (a, b) => ConjugateAMat(a, b) }

  val directSumAMat: P[DirectSumAMat] = P("DirectSumAMat" ~/ "(" ~ aMat.rep(sep = ",") ~ ")")
    .map( seq => DirectSumAMat(seq: _*) )

  val tensorProductAMat: P[TensorProductAMat] = P("TensorProductAMat" ~/ "(" ~ aMat.rep(sep = ",") ~ ")")
    .map( seq => TensorProductAMat(seq: _*) )

  val galoisConjugateAMat: P[GaloisConjugateAMat] = P("GaloisConjugateAMat" ~/ "(" ~ aMat ~ "," ~ int ~ ")")
    .map { case (a, k) => GaloisConjugateAMat(a, k) }


  val aMatMat: P[AMatMat] = P("AMatMat" ~/ "(" ~ "[" ~ cycloVec.rep(sep = ",") ~ "]" ~ ")").map { rows =>
    val nCols = rows.head.length
    val nRows = rows.size
    val raw = rows.map(vec => Seq.tabulate(vec.length)(vec(_))).flatten
    AMatMat(rowMajor[Cyclo](nRows, nCols)(raw: _*))
  }

  val parenMat: P[AMat] = P("(" ~/ aMat ~ ")")

  val aMatSimple: P[AMat] = P(identityPermAMat | aMatMat | aMatPerm | aMatMon | allOneAMat |
    nullAMat | diagonalAMat | dftaMat | soraMat | scalarMultipleAMat | powerAMat |
    conjugateAMat | directSumAMat | tensorProductAMat | galoisConjugateAMat | parenMat)

  val aMatFactor: P[AMat] = P(aMatSimple ~ ("^" ~/ int).?).map {
    case (a, None) => a
    case (a, Some(k)) => PowerAMat(a, k)
  }

  val aMat: P[AMat] = P(aMatFactor ~ ("*" ~/ aMatFactor).rep ).map {
    case (hd, Seq()) => hd
    case (hd, tl) => ProductAMat((hd +: tl): _*)
  }

}

class ParseARep[G:GrpGroup](generator: fastparse.noApi.P[G]) {

  import fastparse.noApi._
  import GapOutput.White._
  import GapOutput._

  val generatorSeq: P[Seq[G]] = P("[" ~ generator.rep(sep = ",") ~ "]")

  val groupWithGenerators: P[Grp[G]] = P("GroupWithGenerators" ~/ "(" ~ generatorSeq ~ ")")
    .map(seq => Grp.fromGenerators(seq.toIndexedSeq))

  val trivialMonARep: P[TrivialMonARep[G]] = P("TrivialMonARep" ~/ "(" ~ groupWithGenerators ~ ")").map(TrivialMonARep(_))

  val trivialPermARep: P[TrivialPermARep[G]] = P("TrivialPermARep" ~/ "(" ~ groupWithGenerators ~ ")").map(TrivialPermARep(_))

  val directSumARep: P[DirectSumARep[G]] = P("DirectSumARep" ~/ "(" ~ aRep.rep(sep = ",") ~ ")").map(DirectSumARep(_: _*))

  val imagesMon: P[Seq[Mon]] = P("[" ~ mon.rep(min = 1, sep = ",") ~ "]")

  val imagesPerm: P[Seq[Perm]] = P("[" ~ perm.rep(sep = ",") ~ "]")

  def parseVec[Scalar](scalar: P[Scalar]): P[Vec[Scalar]] = P("[" ~ scalar.rep(sep = ",") ~ "]")
    .map( (seq: Seq[Scalar]) => vec[Scalar](seq: _*) )

  def parseMat[Scalar](scalar: P[Scalar]): P[Mat[Scalar]] = P("[" ~ parseVec(scalar).rep(min = 1, sep = ",") ~ "]")
    .map { rows =>
      val nRows: Int = rows.size
      val nCols: Int = rows(0).length
      tabulate[Scalar](nRows, nCols)( (r, c) => rows(r)(c) )
    }

  val imagesMat: P[Seq[Mat[Cyclo]]] = P("[" ~ parseMat[Cyclo](cyclo).rep(sep = ",") ~ "]")

  val aRepByImagesMat: P[ARepByImagesMat[G]] =
    P("ARepByImages" ~ "(" ~ groupWithGenerators ~ "," ~ imagesMat ~ ("," ~ "\"hom\"".!).? ~ ")")
    .map { case (grp, images, hint) => ARepByImagesMat(grp, images, images(0).nRows, hint) }
  val aRepByImagesMon: P[ARepByImagesMon[G]] =
    P("ARepByImages" ~ "(" ~ groupWithGenerators ~ "," ~ imagesMon ~ ("," ~ "\"hom\"".!).? ~ ")")
    .map { case (grp, images, hint) => ARepByImagesMon(grp, images, images(0).diag.length, hint) }

  val aRepByImagesPerm: P[ARepByImagesPerm[G]] =
    P("ARepByImages" ~ "(" ~ groupWithGenerators ~ "," ~ imagesPerm ~ "," ~ positiveInt ~ ("," ~ "\"hom\"".!).? ~ ")")
    .map { case (grp, images, dimension, hint) => ARepByImagesPerm(grp, images, dimension, hint) }

  val aRepByImages: P[ARepByImages[G]] = NoCut(aRepByImagesMon) | NoCut(aRepByImagesPerm) | NoCut(aRepByImagesMat)

  val conjugateARep: P[ConjugateARep[G]] = P("ConjugateARep" ~/ "(" ~ aRep ~ "," ~ aMat ~ ")")
    .map { case (r, a) => ConjugateARep(r, a) }

  val aRep = trivialMonARep | trivialPermARep | directSumARep | aRepByImages | conjugateARep

}
*/