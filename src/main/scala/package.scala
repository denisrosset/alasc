package com
import scala.language.implicitConversions
package object faacets {
  import scala.annotation.elidable
  import scala.annotation.elidable._

  @elidable(ASSERTION)
  def require_(requirement: Boolean) {
    if (!requirement)
      throw new java.lang.AssertionError("assumption failed")
  }
  @elidable(ASSERTION)	
  @inline final def require_(requirement: Boolean, message: => Any) {
    if (!requirement)
      throw new IllegalArgumentException("requirement failed: "+ message)
  }

  implicit def asString(l: TeX): String = "$$" + l.s + "$$"

  def isSugared(s: String) = s.startsWith("$$") && s.endsWith("$$")
  def sugar(s: String) = if (isSugared(s)) s else "$$" + s + "$$"
  def unsugar(s: String) = if (isSugared(s)) s.drop(2).dropRight(2) else s
}
