package miniscala
import miniscala.parser.Parser.parse
import miniscala.Unparser.unparse

object Test {
  /**
    * The Test Class for the MiniScala project
    */
  def main(args: Array[String]) = {
    assert(parse(unparse(parse("1+2+3"))) == parse("1+2+3"))

    println("The tests are complete!")
  }

}
