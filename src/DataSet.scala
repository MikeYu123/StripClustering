import java.io.File
import org.json4s._
import org.json4s.jackson.JsonMethods._
import org.json4s.JsonDSL.WithDouble._

import scala.collection.mutable.ListBuffer


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

  def toJsonKM(preparedData: Map[Line, List[Point]]) = {
    val data = preparedData map (x =>
      ("line" -> ("id" -> x._1.hashCode()) ~ ("k" -> x._1.getK) ~ ("b" -> x._1.getB)) ~
      ("points" -> pointsToJson(x._1.hashCode(), x._2))
    )
    compact(render(data))
  }
  def toJsonFEM(preparedData: Map[(Point, Line), Double]) = {
    var data:Map[Point, ListBuffer[(Line, Double)]] = Map[Point, ListBuffer[(Line, Double)]]()
    preparedData foreach (x => {
      if (!data.contains(x._1._1)) data = data + (x._1._1 -> ListBuffer((x._1._2, x._2)))
      else  data(x._1._1) += ((x._1._2, x._2)) // :: data(x._1._1)
    })
    println(data)
    val data2:Map[Line, Map[Point, Line]] = (data map (x => (x._1, x._2.maxBy(_._2)._1))).groupBy(_._2)
    val data3:Map[Line, List[Point]] = data2.map(x=>(x._1, x._2.keys.toList))// map (k,v => k->"loh") //(k:Line, v:Map[Point, Line]) => (v -> v.keys.toList)
    toJsonKM(data3)
    //compact(render(data))
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
    val streets = (data groupBy (p => p.street)) map (x => (x._1, x._2.length)) filter (_._2 > 35)
    val filteredData = data filter (x => streets.keySet.contains(x.street))
    var clusters:Map[(Point, Line), Double] = null
    //do{
      clusters = new FuzzyEMAlgorithm(10).clusterize(filteredData)//new KMeansClusterer(100*5).clusterize(filteredData)

    //} while (clusters.size != 5)

//    clusters = new FuzzyEMAlgorithm(10).clusterize(filteredData)
//    clusters foreach (x => println("y = " + x.getK.toString + "x + " + x.getB.toString()))

    val json = toJsonFEM(clusters)
    println(json)

    write(List(".", "src","data", "result.json").mkString(sep), json)
  }
}