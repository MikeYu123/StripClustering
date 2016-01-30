/**
 * Created by mayur_000 on 29.01.2016.
 */

class FuzzyEMAlgorithm (K: Int){
//  M is the fuzziness exponent
  val M = 1.5
  def randDouble(max: Double, min: Double) = math.random * (max - min) + min

  def initClusters(): List[Line] = {
    (1 to K).toList map (x => new Line(randDouble(3, -3), randDouble(30, -30)))
  }

  def resetLines(lines: List[Line], points: List[Point], memberships: Map[(Point, Line), Double]) : List[Line] = {
    lines map (line => {
//      TODO: Check what happens with memberships with product
      val n = points.length
      val mem = points map (point => (point, memberships.getOrElse((point, line), 0.0))) toMap
      val memSum = (mem map (x => math.pow(x._2, M))) reduce ((x, y) => x + y)
      val memSqSum = (mem map (x => math.pow(x._2, M * 2))) reduce ((x, y) => x + y)
      val sumX = ((points map (point => point.x * mem.getOrElse(point, 0.0))) reduce ((x, y) => x + y)) / memSum
      val sumY = ((points map (point => point.y * mem.getOrElse(point, 0.0))) reduce ((x, y) => x + y)) / memSum
      val sumXSq = ((points map (point => math.pow(point.x * mem.getOrElse(point, 0.0), 2.0))) reduce ((x, y) => x + y)) / memSqSum
      val sumXY = ((points map (point => point.x * point.y * math.pow(mem.getOrElse(point, 0.0), 2.0))) reduce ((x, y) => x + y)) / memSqSum
      val k = ((n * sumXY) - (sumX * sumY)) / ((n * sumXSq) - (sumX * sumX))
      val b = (sumY - (k * sumX)) / n
      new Line(k, b)
    })
  }

  def isSimilar(matrix1 : Map[(Point, Line), Double], matrix2 : Map[(Point, Line), Double]) : Boolean = {
    matrix1.keys forall (x => matrix1.get(x) == matrix2.get(x))
  }

  def countMembershipMatrix(points: List[Point], lines: List[Line]):Map[(Point, Line), Double] = {
    val pointsMembership = points map (point => {
      val memberships = lines map (line => (line, math.pow(1.0 / line.distance(point), 2.0 / M-1)))
      val norm = (memberships map (x => x._2)) reduce ((x, y) => x + y)
      val weighted_memberships = memberships map (x =>(x._1 ,x._2 / norm))
      weighted_memberships map(x => ((point, x._1), x._2)) toMap
    })
    pointsMembership reduce ((x, y) => x ++ y)
  }

  def clusterize(points : List[Point]) : List[Line] = {
    var lines = initClusters()
    var matrix = countMembershipMatrix(points, lines)
    var previousMatrix = Map[(Point, Line), Double]()
    do{
      previousMatrix = matrix
      lines = resetLines(lines, points, matrix)
    } while(!isSimilar(previousMatrix, matrix))
    lines
  }

}

