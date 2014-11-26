package net.alasc.algebra

import spire.algebra.Monoid
import net.alasc.syntax.monoid._

sealed trait Checked {
  def log: Seq[(List[String], String)]
  def withPath(pathElement: String): Checked
  def assert: Unit
}

case class CFail(log: Seq[(List[String], String)]) extends Checked {
  def withPath(pathElement: String) = CFail(log.map { case (path, entry) => (pathElement :: path, entry) })
  override def toString = log.map { case (path, entry) => path.reverse.mkString(".") + ": " + entry }.toString
  def assert: Unit = throw new RuntimeException(toString)
}

object CFail {
  def here(message: String): Checked = CFail(Seq(Nil -> message))
  def hereOn(test: Boolean, message: String) = if (test) here(message) else CSuccess
}

case object CSuccess extends Checked {
  def assert: Unit = { }
  def log = Seq.empty
  def withPath(pathElement: String) = this
}

object Checked {
  def ne[A <: AnyRef](first: A, second: A, prefix: String): Checked =
    if (first ne second) CSuccess else CFail(Seq(Nil -> ((if (prefix != "") s"$prefix " else "") + s"$first eq $second")))
  def eq[A <: AnyRef](first: A, second: A, prefix: String): Checked =
    if (first eq second) CSuccess else CFail(Seq(Nil -> ((if (prefix != "") s"$prefix " else "") + s"$first ne $second")))
  def equals[A](first: A, second: A, prefix: String): Checked =
    if (first == second) CSuccess else CFail(Seq(Nil -> ((if (prefix != "") s"$prefix " else "") + s"$first != $second")))
  def notEquals[A](first: A, second: A, prefix: String = ""): Checked =
    if (first != second) CSuccess else CFail(Seq(Nil -> ((if (prefix != "") s"$prefix " else "") + s"$first == $second")))
  def apply(pathAndResults: (String, Checked)*): Checked =
    (pathAndResults.map { case (pathElement, checked) => checked.withPath(pathElement) }).combine
  implicit val Monoid: Monoid[Checked] = new CheckedMonoid
}

class CheckedMonoid extends Monoid[Checked] {
  def id = CSuccess
  def op(left: Checked, right: Checked) = left match {
    case CSuccess => right
    case CFail(leftLog) => right match {
      case CSuccess => left
      case CFail(rightLog) => CFail(leftLog ++ rightLog)
    }
  }
}

trait Check[T] {
  def check(t: T): Checked
}
