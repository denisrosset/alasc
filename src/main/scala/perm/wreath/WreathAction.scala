package com.faacets.perm
package wreath

import com.faacets.math._

trait WreathAction extends Action {
  type Group = WreathProductGroup
  override val group: WreathProductGroup
  type BottomGroup = group.k.A
  val bottomGroup: BottomGroup = group.k.a
  type BottomElement = bottomGroup.Element
  val bottomAction: Action
  lazy val dims = Vector.fill[Int](group.k.n)(bottomAction.dim)
}
