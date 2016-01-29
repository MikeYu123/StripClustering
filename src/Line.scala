/**
 * Created by mayur_000 on 29.01.2016.
 */
class Line (k: Double, b: Double) {
  def isEqual(line2 : Line) = (k == line2.k) && (b == line2.b)

  def norm:Double = math.sqrt(1 + k*k)

  def distance(p: Point) = math.abs((p.y - k * p.x - b) / norm)
}
