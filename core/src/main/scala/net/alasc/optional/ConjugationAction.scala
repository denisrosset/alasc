package net.alasc.optional

import spire.algebra.{Action, Group}
import spire.syntax.group._

class ConjugationAction[G:Group] extends Action[G, G] {

  def actr(x: G, y: G): G = y.inverse |+| x |+| y

  def actl(y: G, x: G): G = y |+| x |+| y.inverse

}

object ConjugationAction {

  def apply[G:Group] = new ConjugationAction[G]

}
