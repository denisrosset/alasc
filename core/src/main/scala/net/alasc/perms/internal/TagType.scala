package net.alasc.perms.internal

trait TagType

object TagType {

  trait Immutable <: TagType
  trait Mutable[I <: XInt] <: TagType
  trait Scratch[I <: XInt] <: TagType

}
