import java.io.File
import org.json4s._
import org.json4s.jackson.JsonMethods._

object DataSet {
  private val sep = File.separator
  private val rawData:JValue = parse(new File(List(".", "src","data", "data.json").mkString(sep)))

  val data : List[Point] = for {
    JArray(data) <- rawData
    JObject(item) <- data
    JField("name", JString(name)) <- item
    JField("address", JString(address)) <- item
    JField("location", JObject(location)) <- item
    JField("lat", JDouble(lat)) <- location
    JField("lng", JDouble(lng)) <- location

  } yield Point(lat, lng, address, name)

  val clusters = new KMeansClusterer(30).clusterize(data)
  clusters foreach (x => println("y = " + x.getK.toString + "x + " + x.getB.toString()))
//  println()

  def main(args: Array[String]): Unit = {
  }
}