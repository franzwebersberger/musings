package fw.zzz.sphere

import fw.geo.map.projections.Cartesian

object Physics {

  def pot(e0: Double, p1: Cartesian, p2: Cartesian) = e0 + 1.0 / (p1 - p2).norm2

  def systemEnergy(system: Traversable[Cartesian]) = {
    def tailEnergy(e0: Double, p0: Cartesian, tail: Traversable[Cartesian]): Double = tail match {
      case Nil => e0
      case _   => tailEnergy(tail.foldLeft(e0)((e, p) => pot(e, p, p0)), tail.head, tail.tail)
    }
    tailEnergy(0.0, system.head, system.tail)
  }

}
