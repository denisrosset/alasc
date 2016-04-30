package net.alasc.tests
package print

import net.alasc.print.Options

class OptionsSuite extends AlascSuite {

  val color = new Options.Key("OptionsSuite.color") {
    type Value = String
    val red: Value = "red"
    val blue: Value = "blue"
    val green: Value = "green"
    def defaultValue = red
  }

  test("Empty options map returns the default value") {
    Options.empty(color) shouldBe "red"
  }

  test("Options map with key set should return the value set") {
    Options(color := color.blue)(color) shouldBe "blue"

  }

}
