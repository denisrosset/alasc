package net.alasc.algebra

import spire.syntax.semigroup._

class CheckException(errors: Check.Checked) extends RuntimeException(errors.toString) {

  require(errors.nonEmpty)

}

trait Check[A] extends Any {

  import Check._

  def check(a: A): Checked

}

object Check {

  type Path = List[String]

  object Path {
    def empty = List.empty[String]
  }

  type Error = (Path, String)

  type Checked = List[Error]

  def children(pathAndResults: (String, Checked)*): Checked =
    pathAndResults.map { case (pathElement, checked) => checked.withPath(pathElement) }.reduce(_ ++ _)

  def success: Checked = List.empty[Error]

  def failHere(message: String) = List(Path.empty -> message)

  def failHereOn(predicate: Boolean, message: String): Checked =
    if (predicate) failHere(message) else success

  def ne[A <: AnyRef](first: A, second: A, prefix: String): Checked =
    if (first ne second) success else
      failHere((if (prefix != "") s"$prefix " else "") + s"$first eq $second")

  def eq[A <: AnyRef](first: A, second: A, prefix: String): Checked =
    if (first eq second) success else
      failHere((if (prefix != "") s"$prefix " else "") + s"$first ne $second")

  def equals[A](first: A, second: A, prefix: String): Checked =
    if (first == second) success else
      failHere((if (prefix != "") s"$prefix " else "") + s"$first != $second")

  def notEquals[A](first: A, second: A, prefix: String = ""): Checked =
    if (first != second) success else
      failHere((if (prefix != "") s"$prefix " else "") + s"$first == $second")

  implicit class CheckWithPath(val checked: Checked) extends AnyVal {

    def withPath(pathElement: String): Checked = checked.map {
      case (path, message) => (pathElement :: path, message)
    }

    def assert(): Unit = ??? // TODO

  }

}
