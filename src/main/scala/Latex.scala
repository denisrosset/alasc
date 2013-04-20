package com.faacets

/** Trait for objects that have a LaTeX printable representation. */
trait Latex {
  /** Returns LaTeX representation of this object. */
  def latex: String
  override def toString: String = {
    if (Latex.enabled)
      latex
    else
      super.toString
  }
}

object Latex {
  var enabled = false
  def enable { enabled = true }
  def disable { enabled = false }
}
