package net.alasc.math
package bsgs

sealed abstract class AlgorithmType
case object Randomized extends AlgorithmType
case object Deterministic extends AlgorithmType
