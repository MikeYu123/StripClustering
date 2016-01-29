/**
 * Created by mayur_000 on 29.01.2016.
 */
class Line (k: Double, b: Double) {

  def norm:Double = math.sqrt(1 + k*k)

  def distance(p: Point) = math.abs((p.y - k * p.x - b) / norm)
}
