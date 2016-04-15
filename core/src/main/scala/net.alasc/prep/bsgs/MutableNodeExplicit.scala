package net.alasc.prep
package bsgs

import scala.annotation.tailrec
import scala.reflect.ClassTag
import scala.util.Random

import spire.algebra.Group
import spire.syntax.action._
import spire.syntax.group._
import spire.syntax.cfor._

import net.alasc.algebra._
import net.alasc.util._
import metal._
import metal.syntax._

final class MutableNodeExplicit[P](
  var beta: Int,
  var transversal: metal.mutable.HashMap2[Int, P, P],
  var nOwnGenerators: Int,
  var ownGeneratorsArray: Array[P],
  var ownGeneratorsArrayInv: Array[P],
  var prev: MutableStartOrNode[P] = null,
  var next: Chain[P] = null)(implicit val action: FaithfulPermutationAction[P]) extends MutableNode[P] {

  def ownGenerator(i: Int): P = ownGeneratorsArray(i)
  def ownGeneratorInv(i: Int): P = ownGeneratorsArrayInv(i)

  def orbitSize = transversal.longSize.toInt
  def inOrbit(b: Int) = transversal.contains(b)
  def foreachOrbit(f: Int => Unit): Unit = {
    val st = transversal
    @inline @tailrec def rec(ptr: Ptr[st.type]): Unit = ptr match {
      case IsVPtr(vp) =>
        f(vp.key)
        rec(vp.next)
      case _ =>
    }
    rec(st.ptr)
  }
  def orbitIterator = new Iterator[Int] {
    assert(transversal.nonEmpty)
    val tr: metal.generic.HashMap2[Int, P, P] = transversal
    var ptr: Ptr[tr.type] = tr.ptr
    def hasNext = ptr.nonNull
    def next: Int = ptr match {
      case IsVPtr(vp) =>
        val res = vp.key
        ptr = vp.next
        res
      case _ => Iterator.empty.next
    }
  }

  def foreachU(f: P => Unit) = {
    val st = transversal
    @inline @tailrec def rec(ptr: Ptr[st.type]): Unit = ptr match {
      case IsVPtr(vp) =>
        f(vp.value1)
        rec(vp.next)
      case _ =>
    }
    rec(st.ptr)
  }

  def u(b: Int) = transversal.apply1(b)

  def uInv(b: Int) = transversal.apply2(b)

  def randomU(rand: Random): P = u(randomOrbit(rand))

  protected def addTransversalElement(b: Int, u: P, uInv: P): Unit = {
    transversal(b) = (u, uInv)
  }

  protected[bsgs] def addToOwnGenerators(newGen: P, newGenInv: P)(implicit group: Group[P], classTag: ClassTag[P]) = {
    if (nOwnGenerators == ownGeneratorsArray.length) {
      ownGeneratorsArray = arrayGrow(ownGeneratorsArray)
      ownGeneratorsArrayInv = arrayGrow(ownGeneratorsArrayInv)
    }
    ownGeneratorsArray(nOwnGenerators) = newGen
    ownGeneratorsArrayInv(nOwnGenerators) = newGenInv
    nOwnGenerators += 1
  }

  protected[bsgs] def addToOwnGenerators(newGens: Iterable[P], newGensInv: Iterable[P])(implicit group: Group[P], classTag: ClassTag[P]) = {
    val it = newGens.iterator
    val itInv = newGensInv.iterator
    while (it.hasNext) {
      val g = it.next
      val gInv = itInv.next
      addToOwnGenerators(g, gInv)
    }
  }

  /** Remove redundant generators from this node generators. */
  protected[bsgs] def removeRedundantGenerators: Unit = {} /*{
    /* Tests if the orbit stays complete after removing each generator successively. 
     * The redundant generators are removed from the
     * end of the `ownGeneratorsPairs`, the non-redundant are swapped at its beginning, 
     * at the position indicated by `swapHere`.
     */
    val os = orbitSize
    var swapHere = 0
    var ogpLength = ownGenerators.length
    while (swapHere < ogpLength) {
      val newOrbit = MutableBitSet(beta)
      var toCheck = MutableBitSet.empty
      var newAdded = MutableBitSet.empty
      @inline def swapBitsets: Unit = {
        var temp = toCheck
        toCheck = newAdded
        newAdded = temp
        newAdded.clear
      }
      {
        var j = 0
        while (j < ogpLength - 1) {
          val b = beta <|+| ownGenerators(j)
          newOrbit += b
          newAdded += b
          j += 1
        }
      }
      swapBitsets
      while (toCheck.nonEmpty) {
        toCheck.foreachFast { c =>
          var j = 0
          while (j < ogpLength - 1) {
            val cg = c <|+| ownGenerators(j)
            if (!newOrbit.contains(cg)) {
              newOrbit += cg
              newAdded += cg
            }
            j += 1
          }
          next.strongGeneratingSet.foreach { g =>
            val cg = c <|+| g
            if (!newOrbit.contains(cg)) {
              newOrbit += cg
              newAdded += cg
            }
          }
        }
        swapBitsets
      }
      if (newOrbit.size == os)
        ogpLength -= 1
      else {
        var temp = ownGenerators(swapHere)
        ownGenerators(swapHere) = ownGenerators(ogpLength - 1)
        ownGenerators(ogpLength - 1) = temp
        temp = ownGeneratorsInv(swapHere)
        ownGeneratorsInv(swapHere) = ownGeneratorsInv(ogpLength - 1)
        ownGeneratorsInv(ogpLength - 1) = temp
        swapHere += 1
      }
    }
    ownGenerators.reduceToSize(ogpLength)
    ownGeneratorsInv.reduceToSize(ogpLength)
  }*/

  protected[bsgs] def bulkAdd(beta: metal.generic.Buffer[Int], u: metal.mutable.Buffer[P], uInv: metal.mutable.Buffer[P])(implicit group: Group[P]) = {
    cforRange(0 until beta.length.toInt) { i =>
      addTransversalElement(beta(i), u(i), uInv(i))
    }
  }

  protected[bsgs] def updateTransversal(newGen: P, newGenInv: P)(implicit group: Group[P], classTag: ClassTag[P]) = {
    var toCheck = metal.mutable.BitSet.empty
    val toAddBeta = metal.mutable.Buffer.empty[Int]
    val toAddU = metal.mutable.Buffer.empty[P]
    val toAddUInv = metal.mutable.Buffer.empty[P]
    foreachOrbit { b =>
      val newB = b <|+| newGen
      if (!inOrbit(newB)) {
        val newU = u(b) |+| newGen
        val newUInv = newGenInv |+| uInv(b)
        toAddBeta += newB
        toAddU += newU
        toAddUInv += newUInv
        toCheck += newB
      }
    }
    bulkAdd(toAddBeta, toAddU, toAddUInv)
    toAddBeta.clear()
    toAddU.clear()
    toAddUInv.clear()
    while (!toCheck.isEmpty) {
      val newAdded = metal.mutable.BitSet.empty
      val iter = toCheck
      @tailrec def rec1(ptr: Ptr[iter.type]): Unit = ptr match {
        case IsVPtr(vp) =>
          val b = vp.key
          @tailrec def rec(current: Chain[P]): Unit = current match {
            case node: Node[P] =>
              cforRange(0 until node.nOwnGenerators) { i =>
                val g = node.ownGenerator(i)
                val gInv = node.ownGeneratorInv(i)
                val newB = b <|+| g
                if (!inOrbit(newB)) {
                  val newU = u(b) |+| g
                  val newUInv = gInv |+| uInv(b)
                  toAddBeta += newB
                  toAddU += newU
                  toAddUInv += newUInv
                  newAdded += newB
                }
              }
              rec(node.next)
            case _: Term[P] =>
          }
          rec(this)
          rec1(vp.next)
        case _ =>
      }
      rec1(iter.ptr)
      bulkAdd(toAddBeta, toAddU, toAddUInv)
      toAddBeta.clear
      toAddU.clear
      toAddUInv.clear
      toCheck = newAdded
    }
  }

  protected[bsgs] def updateTransversal(newGen: Iterable[P], newGenInv: Iterable[P])(implicit group: Group[P], classTag: ClassTag[P]) = {
    var toCheck = metal.mutable.BitSet.empty
    val toAddBeta = metal.mutable.Buffer.empty[Int]
    val toAddU = metal.mutable.Buffer.empty[P]
    val toAddUInv = metal.mutable.Buffer.empty[P]
    foreachOrbit { b =>
      val gIt = newGen.iterator
      val gInvIt = newGenInv.iterator
      while (gIt.hasNext) {
        val g = gIt.next
        val gInv = gInvIt.next
        val newB = b <|+| g
        if (!inOrbit(newB)) {
          val newU = u(b) |+| g
          val newUInv = gInv |+| uInv(b)
          toAddBeta += newB
          toAddU += newU
          toAddUInv += newUInv
          toCheck += newB
        }
      }
    }
    bulkAdd(toAddBeta, toAddU, toAddUInv)
    toAddBeta.clear
    toAddU.clear
    toAddUInv.clear
    while (!toCheck.isEmpty) {
      val newAdded = metal.mutable.BitSet.empty
      val iter = toCheck
      @inline @tailrec def rec1(ptr: Ptr[iter.type]): Unit = ptr match {
        case IsVPtr(vp) =>
          val b = vp.key
          @inline @tailrec def rec(current: Chain[P]): Unit = current match {
            case node: Node[P] =>
              cforRange(0 until node.nOwnGenerators) { i =>
                val g = node.ownGenerator(i)
                val gInv = node.ownGeneratorInv(i)
                val newB = b <|+| g
                if (!inOrbit(newB)) {
                  val newU = u(b) |+| g
                  val newUInv = gInv |+| uInv(b)
                  toAddBeta += newB
                  toAddU += newU
                  toAddUInv += newUInv
                  newAdded += newB
                }
              }
              rec(node.next)
            case _: Term[P] =>
          }
          rec(this)
          rec1(vp.next)
        case _ =>
      }
      rec1(iter.ptr)
      bulkAdd(toAddBeta, toAddU, toAddUInv)
      toAddBeta.clear
      toAddU.clear
      toAddUInv.clear
      toCheck = newAdded
    }
  }

  protected[bsgs] def conjugate(g: P, gInv: P)(implicit group: Group[P], classTag: ClassTag[P]) = {
    beta = beta <|+| g
    val st = transversal
    val newTransversal = metal.mutable.HashMap2.reservedSize[Int, P, P](st.longSize.toInt)
    @tailrec @inline def rec(ptr: Ptr[st.type]): Unit = ptr match {
      case IsVPtr(vp) =>
        val k = vp.key
        val u = vp.value1
        val newG: P = gInv |+| u |+| g
        val newGInv: P = newG.inverse
        val newB = k <|+| g
        newTransversal(newB) = (newG, newGInv)
        rec(vp.next)
      case _ =>
    }
    rec(st.ptr)
    transversal = newTransversal
    cforRange(0 until nOwnGenerators) { i =>
      ownGeneratorsArray(i) = gInv |+| ownGeneratorsArray(i) |+| g
      ownGeneratorsArrayInv(i) = gInv |+| ownGeneratorsArrayInv(i) |+| g
    }
  }
}

class MutableNodeExplicitBuilder[P] extends NodeBuilder[P] {

  def standaloneClone(node: Node[P])(implicit group: Group[P], classTag: ClassTag[P]) = node match {
    case mne: MutableNodeExplicit[P] =>
      new MutableNodeExplicit(mne.beta, mne.transversal.mutableCopy, mne.nOwnGenerators, mne.ownGeneratorsArray.clone, mne.ownGeneratorsArrayInv.clone)(mne.action)
    case _ => ??? /* TODO
      val newTransversal = SpecKeyMap.fromIterable[Int, InversePair[P]](node.iterable)
      val newOwnGeneratorsPairs = mutable.ArrayBuffer.empty[InversePair[P]] ++= node.ownGeneratorsPairs
      new MutableNodeExplicit(node.beta, newTransversal, newOwnGeneratorsPairs)(node.action)
                   */
  }

  def standalone(beta: Int)(implicit action: FaithfulPermutationAction[P], group: Group[P], classTag: ClassTag[P]) = {
    val transversal = metal.mutable.HashMap2.empty[Int, P, P]
    val id = Group[P].id
    transversal(beta) = (id, id)
    new MutableNodeExplicit(beta,
      transversal,
      0,
      Array.empty[P],
      Array.empty[P])
  }

}
