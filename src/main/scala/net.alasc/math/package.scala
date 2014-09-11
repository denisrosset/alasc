package net.alasc

package object math extends GrpSubgroupsImplicits { // TODO: implicits behavior
  object conjugate extends ConjugateInstances
  object all extends AllInstances
}
