package com.faacets.perm
package wreath

import com.faacets.math._

trait InhImprimitiveWreathAction extends InhWreathAction {
  val dim = dims.sum
  def imageOf(g: group.Element, el: Domain) = {
    val start = dims.scanLeft(0)(_+_)
    val i = start.zipWithIndex.find(_._1 > el).get._2 - 1
    val o = el - start(i)
    val i1 = g.hel.image(i)
    val action = bottomAction(i)
    val o1 = action.imageOf(g.kel.ael(i).asInstanceOf[action.group.Element], o)
    start(i1) + o1
  }
  def imagesOf(g: group.Element) = Array.tabulate(dim) ( el => imageOf(g, el ) )
}
/*
import com.faacets.perm._; import wreath._
val W = new { type A = SymmetricGroup; type H = SymmetricGroup; val a = SymmetricGroup(5); val h = SymmetricGroup(5) } with WreathProductGroup; val C = new { val representedGroup = W } with ImprimitiveWreathAction with BottomIsPermutationGroup
val g1 = C.randomElement; val g2 = C.randomElement; val i1 = Permutation((g1*g2).images); val i2 = Permutation(g1.images)*Permutation(g2.images)
 */
