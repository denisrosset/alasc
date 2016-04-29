package net.alasc.print

import spire.NoImplicit
import spire.algebra.{Eq, MultiplicativeMonoid}
import spire.math.{Rational, SafeLong}
import spire.util.Opt

import cyclo.Cyclo
import scalin.{Mat, Vec}
import scalin.algebra.VecEngine



object Matlab extends Format {

  type Inner = String

  type Output = String

  type Self = Matlab.type

  def finalize(inner: String, options: Options) = inner

}

object Latex extends Format {

  type Inner = String

  type Output = String

  type Self = Latex.type

  def finalize(inner: String, options: Options) = inner

  val fraction = new Options.Key("Latex.fraction") {
    type Value = (String, String) => String
    val tfrac: Value = (n, d) => s"\\tfrac{$n}{$d}"
    val dfrac: Value = (n, d) => s"\\dfrac{$n}{$d}"
    val frac: Value = (n, d) => s"\\frac{$n}{$d}"
    val slash: Value = (n, d) => s"{$n}/{$d}"
    def defaultValue = frac
  }

  val matrixDelimiters = new Options.Key("Latex.matrixDelimiters") {
    type Value = String
    val pmatrix: Value = "pmatrix"
    val bmatrix: Value = "bmatrix"
    def defaultValue = pmatrix
  }

  val expStyle = new Options.Key("Latex.expStyle") {
    type Value = String => String
    val expFunction: Value = arg => s"\\exp \left ( $arg \right )"
    val power: Value = arg => s"\\mathrm{e}^{$arg}"
    def defaultValue = power
  }

  implicit val safeLongLatex: Print[SafeLong, Latex.type] = Print[SafeLong](Latex)( (s, _) => s.toString )

  implicit def rationalAndLatex[A](implicit print: Print[A, Latex.type],
                                   ev: NoImplicit2[Eq[A], MultiplicativeMonoid[A]]): Print[(Rational, A), Latex.type] =
    Print[(Rational, A)](Latex) { case ((r, a),formatter) =>
      if (r.isZero) formatter(r)
      else if (r.isOne) formatter(a)
      else if (r.denominator.isOne) formatter(r.numerator) + " " + formatter(a)
      else if (r.signum < 0) "-" + formatter((-r, a))
      else formatter.options(Latex.fraction)(formatter(r.numerator) + " " + formatter(a), formatter(r.denominator))
    }

  implicit def rationalAndLatexMonoid[A:Eq:MultiplicativeMonoid](implicit print: Print[A, Latex.type]): Print[(Rational, A), Latex.type] =
    Print[(Rational, A)](Latex) { case ((r, a),formatter) =>
      if (r.isZero) formatter(r)
      else if (r.isOne) formatter(a)
      else if (implicitly[MultiplicativeMonoid[A]].isOne(a)) Latex(r)
      else if (r.denominator.isOne) formatter(r.numerator) + " " + formatter(a)
      else if (r.signum < 0) "-" + formatter((-r, a))
      else formatter.options(Latex.fraction)(formatter(r.numerator) + " " + formatter(a), formatter(r.denominator))
    }

  implicit val rationalLatex: Print[Rational, Latex.type] = Print[Rational](Latex) { (r, formatter) =>
    if (r.denominator.isOne) formatter(r.numerator)
    else if (r.signum < 0) "-" + formatter(-r)
    else formatter.options(Latex.fraction)(formatter(r.numerator), formatter(r.denominator))
  }

  implicit val stringLatex: Print[String, Latex.type] = Print[String](Latex)( (s, _) => s )

  implicit val cycloLatex: Print[Cyclo, Latex.type] = Print[Cyclo](Latex) {
    case (Cyclo.Quadratic(a, b, d), formatter) =>

      val termA = a.signum match {
        case 1 | -1 => formatter(a)
        case 0 => ""
      }

      val sqrt = s"\\sqrt{$d}"
      val termB = b.signum match {
        case 1 if termA.isEmpty => formatter((b, sqrt))
        case 1 => " + " + formatter((b, sqrt))
        case -1 if termA.isEmpty => "-" + formatter((-b, sqrt))
        case -1 => " - " + formatter((-b, sqrt))
        case 0 => ""
      }

      if (termA.isEmpty && termB.isEmpty) "0" else termA + termB

    case (c, formatter) =>

      def term(exp: Int, coeff: Rational): String = {
        val r = Rational(2 * exp, c.order)
        val expPi = formatter.options(Latex.expStyle)(formatter((r, "\\pi")))
        formatter((coeff, expPi))
      }

      def leadingTerm(exp: Int, coeff: Rational): String = coeff.signum match {
        case 0 => "0"
        case 1 => term(exp, coeff)
        case -1 => "-" + term(exp, -coeff)
      }

      def followingTerm(exp: Int, coeff: Rational): String = coeff.signum match {
        case 0 => ""
        case 1 => " + " + term(exp, coeff)
        case -1 => " - " + term(exp, -coeff)
      }

      val first = leadingTerm(c.exponent(0), c.coefficient(0))
      val rest = (1 until c.nTerms).map(k => followingTerm(c.exponent(k), c.coefficient(k)))
      (first +: rest).mkString
  }

  case class BlockStructure(blockSizes: Seq[Int]) {

  }

  implicit def matLatexNoBlocks[A](implicit print: Print[A, Latex.type], ev: NoImplicit2[Eq, Ring],
  implicit def matLatexBlocks[A:Eq:Ring](implicit print: Print[A, Latex.type], ev: VecEngine[A, _ <: Vec[A]]): Print[Mat[A], Latex.type] =
    Print[Mat[A]](Latex) { (mat, formatter) =>
      "\\" + formatter.options(Latex.matrixDelimiters) + "{\n" + mat.rowsSeq.map(_.toIndexedSeq.map(formatter(_)).mkString(" & ")).mkString(" \\\\ \n") + "\n}"
    }

  implicit def vecLatex[A](implicit print: Print[A, Latex.type]): Print[Vec[A], Latex.type] =
    Print[Vec[A]](Latex) { (vec, formatter) =>
      "\\" + formatter.options(Latex.matrixDelimiters) + "{ " + vec.toIndexedSeq.map(formatter(_)).mkString(" & ") + " }"
    }

}

trait Print[-A, F <: Format with Singleton] {

  def apply(a: A, options: Options): F#Inner

}

object Print {


  def apply[A](format: Format)(printFunction: (A, Formatter[format.type]) => format.Inner): Print[A, format.type] = new Print[A, format.type] {

    def apply(a: A, options: Options) = printFunction(a, Formatter[format.type](format, options))

  }

}
