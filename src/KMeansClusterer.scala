/**
 * Created by mayur_000 on 29.01.2016.
 */

class KMeansClusterer (k: Int){

  def randDouble(max: Double, min: Double) = math.random * (max - min) + min

  def initClusters(): List[Line] = {
    List(1 to k) map (x => new Line(randDouble(1, -1), randDouble(30, -30)))
  }

  def pickLines(lines: List[Line], points: List[Point]) : Map[Line, List[Point]] = {
    points groupBy(x => pickLine(x, lines))
  }

  def resetLine(points: List[Point]) : Line = {

  }

  def pickLine(point: Point, lines: List[Line]):Line = {
      lines.min(new Ordering[Line] {
        def compare(x:Line,y:Line): Int = x.distance(point)compare y.distance(point)
      })
  }


}

