/**
 * Created by mayur_000 on 29.01.2016.
 */

class KMeansClusterer (k: Int){

  def randDouble(max: Double, min: Double) = math.random * (max - min) + min

  def initClusters(): List[Line] = {
    List(1 to k) map (x => new Line(randDouble(1, -1), randDouble(30, -30)))
  }

  def resetLine(points: List[Point]) : Line = {
    val n = points.length
    val sumxy = (points map (x => x.x * x.y)) reduce ((x, y) => x+y)
    val sumx = points reduce ((x,y) => x.x + y.x)
    val sumy = points reduce ((x,y) => x.y + y.y)
    val sumxsq = points reduce ((x,y) => (x.x*x.x) + (y.x * y.x))
    val k = ((n * sumxy) - (sumx * sumy)) / ((n * sumxsq) - (sumx * sumx))
    val b = (sumy - (k * sumx)) / n
    new Line(k, b)
  }

  def pickLine(point: Point, lines: List[Line]):Line = {
      lines.min(new Ordering[Line] {
        def compare(x:Line,y:Line): Int = x.distance(point)compare y.distance(point)
      })
  }

  def pickLines(lines: List[Line], points: List[Point]) : Map[Line, List[Point]] = {
    points groupBy(x => pickLine(x, lines))
  }

  def resetLines(lineMap : Map[Line, List[Point]]) : List[Line] = {
//    TODO: check if tolist is efficient
    lineMap.values.toList map (points => resetLine(points))
  }

}

