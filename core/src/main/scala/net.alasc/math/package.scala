package net.alasc

import scala.annotation.tailrec

package object math { // TODO: implicits behavior
  object conjugate extends ConjugateInstances
  object all extends AllInstances
  @tailrec def factorial(m: Int, mul: BigInt = 1): BigInt =
    if (m > 1) factorial(m - 1, mul * m) else mul
}
