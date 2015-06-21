package fw.zzz.sphere

import scala.math.Pi
import scala.util.Random

import Physics.systemEnergy
import fw.geo.map.projections.Cartesian
import fw.geo.map.projections.Spherical

object Main extends App {

  val random = Random
  def randLongitude = 2 * Pi * random.nextDouble
  def randLatitude = -0.5 * Pi * random.nextDouble

  Console.println("Main start")

  val d = 2 * Pi / 5

  var emin = 1000.0

  val n = 1000000
  for (i <- 1 to n) {

    val pl = List(
      Spherical(0, 0.5 * Pi),
      Spherical(0, -0.5 * Pi),
      Spherical(0, 0),
      Spherical(d, 0),
      Spherical(randLongitude, 0),
      Spherical(randLongitude, 0),
      Spherical(randLongitude, 0))

    val cl = pl map { p => Cartesian(p) }
    val e = systemEnergy(cl)
    if (e < emin) {
      emin = e
      Console.println(i + "\t" + emin)
      Console.println(pl)
    }
  }

  val p1 = Cartesian(0, 0, 1)
  val p2 = Cartesian(0, 0, -1)
  val p3 = Cartesian(Spherical(0, 0))
  val p4 = Cartesian(Spherical(d, 0))
  val p5 = Cartesian(Spherical(2 * d, 0))
  val p6 = Cartesian(Spherical(3 * d, 0))
  val p7 = Cartesian(Spherical(4 * d, 0))

  val pl = List(p1, p2, p3, p4, p5, p6, p7)

  Console.println(systemEnergy(pl))
}