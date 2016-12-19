package net.alasc.bsgs

import scala.reflect.ClassTag

import spire.algebra.Group

import net.alasc.algebra.PermutationAction

trait NodeBuilder[G] {

  /** Creates a standalone clone of the provided node.
    * 
    * @param node   Node to clone
    */
  def standaloneClone[A <: PermutationAction[G] with Singleton](node: Node[G, A])
                                                                       (implicit group: Group[G], classTag: ClassTag[G]): MutableNode[G, A]

  /** Creates a new standalone BSGS node with a transversal of size 1.
    * 
    * @param beta  New node base point.
    */
  def standalone[A <: PermutationAction[G] with Singleton](beta: Int)
                                                                  (implicit action: A, group: Group[G], classTag: ClassTag[G]): MutableNode[G, A]

}

object NodeBuilder {

  implicit def builder[G]: NodeBuilder[G] = new MutableNodeExplicitBuilder[G]

  def apply[G](implicit ev: NodeBuilder[G]): NodeBuilder[G] = ev

}
