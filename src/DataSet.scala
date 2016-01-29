import java.io.File

import com.fasterxml.jackson.core.{JsonParser, JsonFactory}

object DataSet {
  val str = "Ololo"
  println("hello world")
  var jfactory = new JsonFactory()


  /*** read from file ***/
  var jParser = jfactory.createJsonParser(new File(".\\src\\data\\data.json"))

  def main(args: Array[String]) {
    println(this.str)
  }
}