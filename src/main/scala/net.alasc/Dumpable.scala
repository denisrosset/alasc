package net.alasc

class ParsingFailure(msg: String) extends Exception(msg) { }

trait Dumpable {
  def toText: String
}

trait DumpableCompanion[T <: Dumpable] {
  import util.parsing.combinator.RegexParsers

  trait DumpParser extends RegexParsers {
    def dump: Parser[T]
    def parseText(text: String) = parseAll(dump, text)
  }

  def parser: DumpParser

  def parseText(text: String) = parser.parseText(text)
  def fromText(text: String): T = parseText(text) match {
    case success: DumpParser#Success[T] => success.result
    case error => throw new ParsingFailure(error.toString)
  }
}
