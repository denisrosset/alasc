package com.faacets.perm
package wreath

import com.faacets.math._

trait InhWreathAction extends Action {
  type Group = InhWreathProductGroup
  override val group: InhWreathProductGroup
  type BottomGroup <: FiniteGroup
  val bottomGroupArr: Array[AnyRef] = group.k.aArr
  val bottomActionArr: Array[AnyRef]
  def bottomAction(omega: Domain): Action = bottomActionArr(omega).asInstanceOf[Action]
  def bottomGroup(omega: Domain): BottomGroup = bottomGroupArr(omega).asInstanceOf[BottomGroup]
  lazy val dims = Vector.tabulate[Int](group.k.n)(bottomAction(_).dim)
}
