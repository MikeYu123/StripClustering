import java.io.File
import org.json4s._
import org.json4s.jackson.JsonMethods._
import org.json4s.JsonDSL.WithDouble._


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

  def toJson(preparedData: Map[Line, List[Point]]) = {
    val data = preparedData map (x =>
      ("line" -> ("id" -> x._1.hashCode()) ~ ("k" -> x._1.getK) ~ ("b" -> x._1.getB)) ~
      ("points" -> pointsToJson(x._1.hashCode(), x._2))
    )
    compact(render(data))
  }

  def pointsToJson(lineNumber: Int, list: List[Point]) = list map (x =>
      ("name" -> x.name) ~
      ("address" -> x.street) ~
      ("lat" -> x.x) ~
      ("lng" -> x.y) ~
      ("cluster" -> lineNumber)
  )




  def write(path: String, txt: String): Unit = {
    import java.nio.file.{Paths, Files}
    import java.nio.charset.StandardCharsets

    Files.write(Paths.get(path), txt.getBytes(StandardCharsets.UTF_8))
  }

  def main(args: Array[String]): Unit = {
    val streets = (data groupBy (p => p.street)) map (x => (x._1, x._2.length)) filter (_._2 > 15)
    val filteredData = data filter (x => streets.keySet.contains(x.street))

    val clusters = new KMeansClusterer(1000000).clusterize(filteredData)
    clusters.keys foreach (x => println("y = " + x.getK.toString + "x + " + x.getB.toString()))

    val json = toJson(clusters)
    println(json)

    write(List(".", "src","data", "result.json").mkString(sep), json)
  }
}