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
      points groupBy(x => pickLine(x, lines))
  }

  def resetLines(lineMap : Map[Line, List[Point]]) : List[Line] = {
//    TODO: check if tolist is efficient
    lineMap.values.toList map (points => resetLine(points))
  }

  def isSimilar(list1 : List[Line], list2 : List[Line]) = {
//  TODO: is exact match efficient?
    list1.forall(line1 => list2.count(line2 => line1.isEqual(line2)) > 0)
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

