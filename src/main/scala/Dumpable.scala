package com.faacets

trait Dumpable {
  def toTextDump: String
}

trait DumpableCompanion[T <: Dumpable] {
  def fromTextDump(dump: String): Option[T]
}