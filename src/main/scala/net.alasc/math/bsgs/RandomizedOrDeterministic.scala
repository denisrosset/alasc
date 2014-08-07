package net.alasc.math
package bsgs

sealed abstract class RandomizedOrDeterministic
case object Randomized extends RandomizedOrDeterministic
case object Deterministic extends RandomizedOrDeterministic
