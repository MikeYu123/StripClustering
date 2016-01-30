/**
 * Created by mayur_000 on 29.01.2016.
 */

class KMeansClusterer (K: Int){

  def randDouble(max: Double, min: Double) = math.random * (max - min) + min

  def initClusters(): List[Line] = {
    (1 to K).toList map (x => new Line(randDouble(3, -3), randDouble(30, -30)))
  }

  def resetLine(points: List[Point]) : Line = {
    val n = points.length
    val sumxy : Double = (points map (x => x.x * x.y)) reduceLeft ((x, y) => x+y)
    val sumx : Double = (points map (x => x.x)) reduceLeft ((x,y) => x + y)
    val sumy : Double = (points map (x => x.y)) reduceLeft ((x,y) => x + y)
    val sumxsq : Double = (points map (x => x.x * x.x)) reduceLeft ((x,y) => x + y)
    val k = ((n * sumxy) - (sumx * sumy)) / ((n * sumxsq) - (sumx * sumx))
    val b = (sumy - (k * sumx)) / n
    new Line(k, b)
  }

  def pickLine(point: Point, lines: List[Line]):Line = {
      lines minBy(x => x.distance(point))
  }

  def pickLines(lines: List[Line], points: List[Point]) : Map[Line, List[Point]] = {
      points groupBy(x => getLine(points, x, lines))//pickLine(x, lines)
  }
  def getLine(points: List[Point], p:Point, lines: List[Line]):Line = {
    val line = pickLine(p, lines)
//    val line_d = line.distance(p)
//    val nearestPoints = points takeWhile (point => p != point && 2*line_d > math.sqrt(math.pow(p.x - point.x, 2.0) + math.pow(p.y - point.y, 2.0)))
//
//    val npFreq = nearestPoints groupBy (pickLine(_, lines)) map(x=> (x._1, x._2.length))
//
//    val candidate = if (npFreq.nonEmpty) npFreq maxBy (x => x._2) else null
////    if (candidate != null) println(candidate._2)
//    if (candidate != null &&  candidate._2 > 1) {
//      println("WIN")
//      line = candidate._1
//    }
    line
  }

  def resetLines(lineMap : Map[Line, List[Point]]) : List[Line] = {
//    TODO: check if tolist is efficient
    lineMap.values.toList map (points => resetLine(points))
  }

  def isSimilar(list1 : List[Line], list2 : List[Line]) = {
//  TODO: is exact match efficient?
    list1.forall(line1 => list2 exists (line2 => line1.isEqual(line2)))
  }

  def clusterize(points : List[Point]) : Map[Line, List[Point]] = {
    var lines = initClusters()
    var previousLines = List[Line]()
    do{
      previousLines = lines
      lines = resetLines(pickLines(previousLines, points))
    } while(!isSimilar(previousLines, lines))
    pickLines(lines, points)
  }

}

