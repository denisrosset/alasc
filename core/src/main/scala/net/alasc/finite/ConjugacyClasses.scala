package net.alasc.finite

import spire.algebra.Group
import spire.math.SafeLong
import spire.util.Opt
import net.alasc.syntax.group._

case class ConjugacyClass[G](grp: Grp[G], g: G, centralizer: Grp[G]) {
  def size: SafeLong = grp.order / centralizer.order
}

case class ConjugacyClasses[G](grp: Grp[G], classes: Seq[ConjugacyClass[G]])

object ConjugacyClasses {

  def apply[G:GrpGroup](grp: Grp[G]): ConjugacyClasses[G] = {
    import grp.{equ, group}
    val classes = scala.collection.mutable.ArrayBuffer.empty[ConjugacyClass[G]]
    classes += ConjugacyClass(grp, Group[G].id, grp)
    var size: SafeLong = SafeLong.one
    val go = grp.order
    while (size < go) {
      val r = grp.randomElement(scala.util.Random)
      if (!classes.exists(cl => grp.areConjugate(r, cl.g, Opt(cl.centralizer)))) {
        val newClass = ConjugacyClass(grp, r, grp.centralizer(r))
        size += newClass.size
        classes += newClass
      }
    }
    ConjugacyClasses(grp, classes.toVector)
  }

}
