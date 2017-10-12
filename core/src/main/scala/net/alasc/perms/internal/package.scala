package net.alasc.perms

package object internal {

  object instances extends InternalInstances
  object syntax extends InternalSyntax
  object implicits extends InternalInstances with InternalSyntax

  type XInt = Int with Singleton

  type GenPrm = Array[Int] { type Tag <: TagType }

  type Prm = Array[Int] { type Tag = TagType.Immutable }

  type MutPrm[I <: XInt] = Array[Int] { type Tag = TagType.Mutable[I] }

  type TmpPrm[I <: XInt] = Array[Int] { type Tag = TagType.Scratch[I] }

}
