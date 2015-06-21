package fw.geo.map.projections

import scala.math.cos
import scala.math.sin
import scala.math.sqrt

case class Cartesian(x: Double, y: Double, z: Double) {
  def +(p: Cartesian) = Cartesian(x + p.x, y + p.y, z + p.z)
  def -(p: Cartesian) = Cartesian(x - p.x, y - p.y, z - p.z)
  def norm2 = x * x + y * y + z * z
  def norm = sqrt(norm2)
}

object Cartesian {
  def apply(p: Spherical): Cartesian = Cartesian(
    p.radius * cos(p.latitude) * cos(p.longitude),
    p.radius * cos(p.latitude) * sin(p.longitude),
    p.radius * sin(p.latitude))
}
