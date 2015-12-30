package net.alasc.math
package bsgs

import scala.reflect.ClassTag

import spire.algebra.Group

import net.alasc.algebra.{FaithfulPermutationAction}

trait NodeBuilder[P] {

  /** Creates a standalone clone of the provided node.
    * 
    * @param node   Node to clone
    */
  def standaloneClone(node: Node[P])(implicit group: Group[P], classTag: ClassTag[P]): MutableNode[P]

  /** Creates a new standalone BSGS node with a transversal of size 1.
    * 
    * @param beta  New node base point.
    */
  def standalone(beta: Int)(implicit action: FaithfulPermutationAction[P], group: Group[P], classTag: ClassTag[P]): MutableNode[P]

}
