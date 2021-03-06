/**
 * Created by mayur_000 on 29.01.2016.
 */

class FuzzyEMAlgorithm (K: Int){
//  M is the fuzziness exponent
  val M = 1.3
  def randDouble(max: Double, min: Double) = math.random * (max - min) + min

  def initClusters(points: List[Point]): List[Line] = {
//    (1 to K).toList map (x => new Line(randDouble(3, -3), randDouble(30, -30)))
    points.grouped(20).toList map (x => new Line(resetLine(x).getK*randDouble(0.9, 1.1),resetLine(x).getB*randDouble(0.9, 1.1)))
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
//      val sumXSq = ((points map (point => math.pow(point.x * mem.getOrElse(point, 0.0), 1.0))) reduce ((x, y) => x + y)) / memSum
//      val sumXY = ((points map (point => point.x * point.y * math.pow(mem.getOrElse(point, 0.0), 1.0))) reduce ((x, y) => x + y)) / memSum
      val k = ((n * sumXY) - (sumX * sumY)) / ((n * sumXSq) - (sumX * sumX))
      val b = (sumY - (k * sumX)) / n
      new Line(k, b)
    })
  }

  def isSimilar(matrix1 : Map[(Point, Line), Double], matrix2 : Map[(Point, Line), Double]) : Boolean = {
     matrix1.keys forall (x => plSimilar(x, matrix1, matrix2))
  }
  def plSimilar(x:(Point, Line), m1: Map[(Point, Line), Double], m2:Map[(Point, Line), Double]):Boolean = {
    var res = false
    val m2_elems = m2.filter(y => y._1._1 == x._1 && y._1._2.isEqual(x._2))
//    println(x, m2_elems)
    if (m2_elems.nonEmpty){
      val (_, m2_value) = m2_elems.head
      val abs = math.abs(m1(x) - m2_value)
//      println(abs)
      res =  abs < 0.5
    }

    res
  }
  def diff(matrix1 : Map[(Point, Line), Double], matrix2 : Map[(Point, Line), Double]) : Double = {
    (matrix1 map (x => math.abs(x._2 - matrix2.getOrElse(x._1, 0.0)))) reduce((x, y) => x + y)
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

  def clusterize(points : List[Point]): Map[(Point, Line), Double] = {
    var lines = initClusters(points)
    var matrix = countMembershipMatrix(points, lines)
    var previousMatrix = Map[(Point, Line), Double]()
    var counter = 0
    do{
      println("================" + counter)
      counter += 1
      previousMatrix = matrix
      lines = resetLines(lines, points, matrix)
      matrix = countMembershipMatrix(points, lines)
      println(isSimilar(matrix, previousMatrix))
    } while(!isSimilar(matrix, previousMatrix))
//    } while(counter < 10)
    println(matrix)
    matrix
  }

}

