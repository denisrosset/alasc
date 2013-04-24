package com.faacets.perm
package wreath

import com.faacets.math._

trait PrimitiveWreathAction extends WreathAction {
  val dim = dims.product
  def imageOf(g: group.Element, el: Domain) = {
    val alpha = ind2sub(dims, el)
    val alpha1 = new Array[Int](alpha.size)
    for (i <- 0 until alpha.size)
      alpha1(g.hel.image(i)) = bottomAction.imageOf(g.kel.ael(i).asInstanceOf[bottomAction.group.Element], alpha(i))
    sub2ind(dims, alpha1)
  }
  def imagesOf(g: group.Element) = Array.tabulate(dim) ( el => imageOf(g, el ) )
}
/*
import com.faacets.perm._; import wreath._
val W = new { type A = SymmetricGroup; type H = SymmetricGroup; val a = SymmetricGroup(3); val h = SymmetricGroup(3) } with WreathProductGroup; val C = new { val representedGroup = W } with PrimitiveWreathAction with BottomIsPermutationGroup
val g1 = C.randomElement; val g2 = C.randomElement; val i1 = Permutation((g1*g2).images); val i2 = Permutation(g1.images)*Permutation(g2.images)
 */
