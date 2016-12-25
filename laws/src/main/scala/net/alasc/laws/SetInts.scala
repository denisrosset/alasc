package net.alasc.laws

import org.scalacheck.{Arbitrary, Gen}
import Arbitrary.arbitrary

import net.alasc.domains.Domain

object SetInts {

  @inline def rightFillMask(n: Int): Long = ((1L << n) - 1) - ((n & 64) >> 6)

  def inDomain(domain: Domain): Gen[Set[Int]] = {
    import metal.generic.BitSet.WordLength
    val nWords = (domain.size - 1) / WordLength + 1
    Gen.containerOfN[Array, Long](nWords, arbitrary[Long]) flatMap {
      wordArray =>
        val finalArray = wordArray.clone // TODO do we need a defensive copy?
        finalArray(nWords - 1) &= rightFillMask(domain.size % WordLength)
        metal.immutable.BitSet.fromBitmaskNoCopy(finalArray, nWords).toScala // TODO does Scala BitSet API have a contract on the word length?
    }
  }

}
