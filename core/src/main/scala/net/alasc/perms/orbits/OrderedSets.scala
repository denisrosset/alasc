package net.alasc.perms.orbits

import spire.algebra.Group
import spire.syntax.action._
import spire.syntax.cfor._
import spire.util.Opt

import net.alasc.algebra.{PermutationAction}
import net.alasc.bsgs.{BaseChange, BaseGuideLex, BaseGuideSeq, BaseOrder, BaseSwap, BuildChain, Chain, GrpChain, Node, SchreierSims, SubgroupSearch, Term}
import net.alasc.finite.Grp
import net.alasc.perms.{MutableOrbit, PermGrpChainAlgos, orbits}
import net.alasc.syntax.permutationAction._
import spire.syntax.group._
import net.alasc.util.NNOption

import metal.syntax._

object OrderedSets {

  @inline def booleanToInt(b: Boolean): Int = if (b) 1 else 0

  // we define true < false
  def compareBoolean(lhs: Boolean, rhs: Boolean): Int = -(booleanToInt(lhs) - booleanToInt(rhs))

  //TODO: restore ordered sets
/*
  /** Returns the minimal lexicographic representative of a set of integers under permutation.
    *
    * @param set       Set of nonnegative integers.
    * @param grp       Group of permutations
    * @param symGrpOpt Optionl subgroup of `grp` leaving `set` invariant, i.e.
    *                  for all `h` in `symGrpOpt`, `set(i <|+| h) = set(i)`
    * @return the permutation `g` in `grp` such that `set <|+| g` is the lexicographic minimal ordered set.
    */
  def toSmallest[G, F <: Permutation[G] with Singleton](set: Set[Int], grp: Grp[G], symGrpOpt: Opt[Grp[G]] = Opt.empty[Grp[G]])
                                                       (implicit builder: PermGrpChainAlgos[G, F]): G = {
    import builder.{baseChange, baseSwap, schreierSims}
    import builder.{classTag, permutation}
    import spire.std.int._
    val n: Int = grp.largestMovedPoint match {
      case NNOption(nval) => nval + 1
      case _ => return permutation.id
    }
    val grpChain = builder.fromGrp(grp, Opt(BaseGuideLex(n)))
    val symGrpChain: GrpChain[G, F] = symGrpOpt match {
      case Opt(symGrp) => builder.fromGrp(grp)
      case _ => builder.setwiseStabilizer(grpChain, set)
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
    def rec(level: Int, toLevel: Int, curG: G, curChainGrp: Chain[G, F], curSymGrp: GrpChain[G, F], sOrbit: MutableOrbit): Unit =
      curChainGrp match {
        case node: Node[G, F] if level <= toLevel =>
          val candidates = metal.mutable.Buffer.empty[Int]
          val beta = node.beta
          val nextBeta = node.next match {
            case nextNode: Node[G, F] => nextNode.beta
            case _: Term[G, F] => n
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
  def isSmallestInOrbit[G, F <: Permutation[G] with Singleton](set: Set[Int], grp: Grp[G], symGrpOpt: Opt[Grp[G]] = Opt.empty[Grp[G]])
                                                              (implicit builder: PermGrpChainAlgos[G, F]): Boolean = {
    import builder.{baseChange, baseSwap, schreierSims}
    import builder.{classTag, permutation}
    import spire.std.int._
    val n: Int = grp.largestMovedPoint match {
      case NNOption(nval) => nval + 1
      case _ => return true
    }
    val grpChain = builder.fromGrp(grp, Opt(BaseGuideLex(n)))
    val symGrpChain: GrpChain[G, F] = symGrpOpt match {
      case Opt(symGrp) => builder.fromGrp(grp)
      case _ => builder.setwiseStabilizer(grpChain, set)
    }
    val bitset = metal.immutable.BitSet.fromIterable(set)
    var isMinimal = true
    val sOrbit = MutableOrbit.forSize(n)
    assert(grpChain.chain.hasLexicographicBase) // TODO remove
    val lexChain = grpChain.chain
    // Implements breadth-first search in the cosets `symGrp \ grp`, filtering elements that do not lead to a minimal
    // lexicographic representative at each step in the stabilizer chain.
    def rec(level: Int, toLevel: Int, curG: G, curChainGrp: Chain[G, F], curSymGrp: GrpChain[G, F], sOrbit: MutableOrbit): Unit =
      curChainGrp match {
        case node: Node[G, F] if level <= toLevel =>
          val candidates = metal.mutable.Buffer.empty[Int]
          val beta = node.beta
          val nextBeta = node.next match {
            case nextNode: Node[G, F] => nextNode.beta
            case _: Term[G, F] => n
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
  }*/

}
