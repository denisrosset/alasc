package net.alasc.perms.internal

trait TagType

object TagType {

  trait Immutable extends TagType
  trait Mutable[I <: XInt] extends TagType
  trait Scratch[I <: XInt] extends TagType

}
