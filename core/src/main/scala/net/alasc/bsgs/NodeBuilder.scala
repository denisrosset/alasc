package net.alasc.bsgs

import scala.reflect.ClassTag

import spire.algebra.Group

import net.alasc.algebra.FaithfulPermutationAction

trait NodeBuilder[G] {

  /** Creates a standalone clone of the provided node.
    * 
    * @param node   Node to clone
    */
  def standaloneClone[F <: FaithfulPermutationAction[G] with Singleton](node: Node[G, F])
                                                                       (implicit group: Group[G], classTag: ClassTag[G]): MutableNode[G, F]

  /** Creates a new standalone BSGS node with a transversal of size 1.
    * 
    * @param beta  New node base point.
    */
  def standalone[F <: FaithfulPermutationAction[G] with Singleton](beta: Int)
                                                                  (implicit action: F, group: Group[G], classTag: ClassTag[G]): MutableNode[G, F]

}

object NodeBuilder {

  implicit def builder[G]: NodeBuilder[G] = new MutableNodeExplicitBuilder[G]

  def apply[G](implicit ev: NodeBuilder[G]): NodeBuilder[G] = ev

}
