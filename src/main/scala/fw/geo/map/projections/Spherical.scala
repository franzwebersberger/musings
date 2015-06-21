package fw.geo.map.projections

case class Spherical(radius: Double, longitude: Double, latitude: Double)

object Spherical {
  def apply(longitude: Double, latitude: Double): Spherical = Spherical(1.0, longitude, latitude) 
}
