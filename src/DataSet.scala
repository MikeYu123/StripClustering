import java.io.File

import com.fasterxml.jackson.core.{JsonParser, JsonFactory}

object DataSet {
  val str = "Ololo"
  println("hello world")
  var jfactory = new JsonFactory()

  val sep = File.separator
  /*** read from file ***/
  var jParser = jfactory.createJsonParser(new File(List(".", "src","data", "data.json").mkString(sep)))

  def main(args: Array[String]) {
    println(this.str)
  }
}