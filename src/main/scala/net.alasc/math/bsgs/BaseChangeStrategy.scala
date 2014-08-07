package net.alasc.math
package bsgs

/** Base class for base change strategies. */
sealed abstract class BaseChangeStrategy

/** Use only swapping and insertion of new base elements. */
case object BaseSwapOnly extends BaseChangeStrategy
/** Use both swapping, insertion and conjugation. */
case object BaseSwapAndConjugation extends BaseChangeStrategy
/** Always rebuild the base from scratch using randomized/deterministic Schreier-Sims algorithm. */
case object BaseFromScratch extends BaseChangeStrategy
/** Select one strategy using heuristics. */
case object BaseHeuristic extends BaseChangeStrategy
