/**
 * Created by mayur_000 on 29.01.2016.
 */
class Line (k: Double, b: Double) {
  def getK: Double = k
  def getB: Double = b

  def isEqual(line2 : Line): Boolean = (k == line2.getK) && (b == line2.getB)

  def norm:Double = math.sqrt(1 + k*k)

  def distance(p: Point) = math.abs((p.y - k * p.x - b) / norm)
}
