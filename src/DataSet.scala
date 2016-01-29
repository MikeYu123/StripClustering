import java.io.File
import org.json4s._
import org.json4s.jackson.JsonMethods._

object DataSet {
  val data = parse(new File(List(".", "src","data", "data.json").mkString(sep)))

  def main(args: Array[String]) {

  }
}