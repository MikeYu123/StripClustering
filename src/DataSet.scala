import java.io.File
import org.json4s._
import org.json4s.jackson.JsonMethods._

object DataSet {
  val data = parse(new File("./src/data/data.json"))

  def main(args: Array[String]) {

  }
}