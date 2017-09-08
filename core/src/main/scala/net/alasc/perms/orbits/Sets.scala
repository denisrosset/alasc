package net.alasc.perms.orbits

import spire.algebra.Group
import spire.syntax.action._
import spire.syntax.cfor._
import spire.syntax.group._
import spire.util.Opt

import net.alasc.algebra.PermutationAction
import net.alasc.bsgs.{BaseGuideLex, Chain, GrpChain, GrpChainPermutationAction, Node, Term}
import net.alasc.finite.Grp
import net.alasc.perms.{MutableOrbit, orbits}
import net.alasc.syntax.group._
import net.alasc.util.NNOption

object Sets {

  @inline def booleanToInt(b: Boolean): Int = if (b) 1 else 0

  // we define true < false
  def compareBoolean(lhs: Boolean, rhs: Boolean): Int = -(booleanToInt(lhs) - booleanToInt(rhs))

  /** Returns the minimal lexicographic representative of a set of integers under permutation.
    *
    * @param grp       Group of permutations
    * @param action
    * @param set       Set of nonnegative integers.
    * @param symGrpOpt Optionl subgroup of `grp` leaving `set` invariant, i.e.
    *                  for all `h` in `symGrpOpt`, `set(i <|+| h) = set(i)`
    * @return the permutation `g` in `grp` such that `set <|+| g` is the lexicographic minimal ordered set.
    */
  def toSmallest[G](grp: Grp[G], action: PermutationAction[G], set: Set[Int], symGrpOpt: Opt[Grp[G]] = Opt.empty[Grp[G]])
                                                             (implicit gcpa: GrpChainPermutationAction[G]): G = {
    implicit def ia: action.type = action
    import gcpa.{baseSwap, group}
    val n: Int = grp.largestMovedPoint(action) match {
      case NNOption(nval) => nval + 1
      case _ => return group.id
    }
    val grpChain = gcpa.fromGrp(grp, action, Opt(BaseGuideLex(n)))
    val symGrpChain: GrpChain[G, action.type] = symGrpOpt match {
      case Opt(symGrp) => gcpa.fromGrp(symGrp, action)
      case _ => gcpa.setwiseStabilizer(grpChain, action, set)
    }
    val bitset = metal.immutable.BitSet.fromIterable(set)
    val minimal = metal.mutable.FixedBitSet.reservedSize(n)
    var minimalCorrectBefore = 0
    var minimalG = Group[G].id
    val sOrbit = MutableOrbit.forSize(n)
    assert(grpChain.chain.hasLexicographicBase) // TODO remove
    val lexChain = grpChain.chain
    // Implements breadth-first search in the cosets `symGrp \ grp`, filtering elements that do not lead to a minimal
    // lexicographic representative at each step in the stabilizer chain.
    def rec(level: Int, toLevel: Int, curG: G, curChainGrp: Chain[G, action.type], curSymGrp: GrpChain[G, action.type], sOrbit: MutableOrbit): Unit =
      curChainGrp match {
        case node: Node[G, action.type] if level <= toLevel =>
          val candidates = metal.mutable.Buffer.empty[Int]
          val beta = node.beta
          val nextBeta = node.next match {
            case nextNode: Node[G, action.type] => nextNode.beta
            case _: Term[G, action.type] => n
          }
          if (nextBeta > minimalCorrectBefore) {
            cforRange(minimalCorrectBefore until nextBeta) { k =>
              minimal(k) = bitset(k <|+| minimalG)
            }
            minimalCorrectBefore = nextBeta
          }
          node.foreachOrbit { b =>
            val bg = b <|+| curG
            var comp = compareBoolean(bitset(bg), minimal(beta))
            var k = beta + 1
            lazy val nextG = node.u(b) |+| curG
            while (k < nextBeta && comp == 0) {
              comp = compareBoolean(bitset(k <|+| nextG), minimal(k))
              k += 1
            }
            if (comp <= 0) {
              if (comp < 0) {
                cforRange(beta until minimalCorrectBefore) { k =>
                  minimal(k) = bitset(k <|+| nextG)
                }
                minimalG = nextG
                candidates.clear
              }
              candidates += b
            }
          }
          cforRange(0 until candidates.length.toInt) { i =>
            val b = candidates(i)
            val bg = b <|+| curG
            if (orbits.Points.isSmallestInOrbit(bg, curSymGrp.generators, Opt(sOrbit))(implicitly, spire.std.int.IntAlgebra)) {
              val nextG = node.u(b) |+| curG
              rec(level + 1, toLevel, nextG, node.next, GrpChain.stabilizer(curSymGrp, bg), sOrbit)
            }
          }
        case _ =>
      }
    cforRange(0 until lexChain.length) { i =>
      rec(0, i, Group[G].id, lexChain, symGrpChain, sOrbit)
    }
    minimalG.inverse
  }

  /** Returns whether this set is the minimal lexicographic representative in its orbit under a group.
    *
    * @param set       Set of nonnegative integers.
    * @param grp       Group of permutations
    * @param symGrpOpt Optionl subgroup of `grp` leaving `set` invariant, i.e.
    *                  for all `h` in `symGrpOpt`, `set(i <|+| h) = set(i)`
    * @return whether set is lexicographically minimal compared to all set <|+| g for g in grp.
    */
  def isSmallestInOrbit[G](grp: Grp[G], action: PermutationAction[G], set: Set[Int], symGrpOpt: Opt[Grp[G]] = Opt.empty[Grp[G]])
                                                              (implicit gcpa: GrpChainPermutationAction[G]): Boolean = {
    import gcpa.{baseSwap, group}
    implicit def ia: action.type = action
    val n: Int = grp.largestMovedPoint(action) match {
      case NNOption(nval) => nval + 1
      case _ => return true
    }
    val grpChain = gcpa.fromGrp(grp, action, Opt(BaseGuideLex(n)))
    val symGrpChain: GrpChain[G, action.type] = symGrpOpt match {
      case Opt(symGrp) => gcpa.fromGrp(symGrp, action)
      case _ => gcpa.setwiseStabilizer(grpChain, action, set)
    }
    val bitset = metal.immutable.BitSet.fromIterable(set)
    var isMinimal = true
    val sOrbit = MutableOrbit.forSize(n)
    assert(grpChain.chain.hasLexicographicBase) // TODO remove
    val lexChain = grpChain.chain
    // Implements breadth-first search in the cosets `symGrp \ grp`, filtering elements that do not lead to a minimal
    // lexicographic representative at each step in the stabilizer chain.
    def rec(level: Int, toLevel: Int, curG: G, curChainGrp: Chain[G, action.type], curSymGrp: GrpChain[G, action.type], sOrbit: MutableOrbit): Unit =
      curChainGrp match {
        case node: Node[G, action.type] if level <= toLevel =>
          val candidates = metal.mutable.Buffer.empty[Int]
          val beta = node.beta
          val nextBeta = node.next match {
            case nextNode: Node[G, action.type] => nextNode.beta
            case _: Term[G, action.type] => n
          }
          node.foreachOrbit { b =>
            val bg = b <|+| curG
            var comp = compareBoolean(bitset(bg), bitset(beta))
            var k = beta + 1
            if (k < nextBeta && comp == 0) {
              val nextG = node.u(b) |+| curG
              while (k < nextBeta && comp == 0) {
                comp = compareBoolean(bitset(k <|+| nextG), bitset(k))
                k += 1
              }
            }
            if (comp < 0) {
              isMinimal = false
              return
            }
            if (comp == 0)
              candidates += b
          }
          cforRange(0 until candidates.length.toInt) { i =>
            val b = candidates(i)
            val bg = b <|+| curG
            if (orbits.Points.isSmallestInOrbit(bg, curSymGrp.generators, Opt(sOrbit))(implicitly, spire.std.int.IntAlgebra)) {
              val nextG = node.u(b) |+| curG
              rec(level + 1, toLevel, nextG, node.next, GrpChain.stabilizer(curSymGrp, bg), sOrbit)
            }
          }
        case _ =>
      }
    cforRange(0 until lexChain.length) { i =>
      rec(0, i, Group[G].id, lexChain, symGrpChain, sOrbit)
      if (!isMinimal) return isMinimal
    }
    isMinimal
  }

}
