package fw.ct.nerdcube

import scala.util.Random
import java.util.Date

/**
 * @see <a href="http://www.heise.de/ct/projekte/vertrac-t-Der-c-t-NerdCube-2765682.html">Der c't-NerdCube</a>
 */
object Solver extends App {
  import Cube._

  // Der c't-NerdCube
  val cube: Cube = """
         C> E^ H-
         E< O| T<
         R< T> D<
S| C^ Pv C> D> P< I- D< T^
M< S- Pv P^ '^ S| S| P> M<
Av T> Pv D> F^ I| X| S| O-
         Fv P< O-
         D< I| Cv
         P^ C< S|
         R> Cv S-
         I- O| Pv
         I- V^ F>    
    """

  // initial move: solves U face
  val initialMove: Move = """
    B' D L' F D' F L' F' U' B' U
    B' D B D' F D F'
    B' D2 B F' D' F
    L B' L' B L B' L' B
    D2 R' B R B'
    """

  // center flip (here: B face as real rubics cube moves)
  val centerFlips = Seq[Move](
    "R B R' B R B2 R' D B D' B D B2 D' B2 R B' L' B R' B' L B D B' U' B D' B' U B",
    Move(Perm(), Seq(0, 2, 0, 0, 0, 0)),
    Move(Perm(), Seq(0, 0, 2, 0, 0, 0)),
    Move(Perm(), Seq(0, 0, 0, 2, 0, 0)),
    Move(Perm(), Seq(0, 0, 0, 0, 2, 0)),
    Move(Perm(), Seq(0, 0, 0, 0, 0, 2)))

  val testMoves1 = Seq[Move](
    "F D' F' D' R' D R",
    "R D' R' D' B' D B",
    "B D' B' D' L' D L",
    "L D' L' D' F' D F",
    "R' D R D F D' F'",
    "B' D B D R D' R'",
    "L' D L D B D' B'",
    "F' D F D L D' L'",
    "D")

  val testMoves2 = Seq[Move](
    "L' D R D' L D R' D'",
    "L B R' B' L' B R B'",
    "L' D B D' F2 D B' D' F2 L",
    "D")

  // apply initial move
  cube > initialMove

  var it: Long = 0
  val r = Random
  val l1Max = 1000

  // enter infinite loop
  while (true) {
    it += 1
    // apply random move from testMoves1
    cube > testMoves1(r.nextInt(testMoves1.size))
    if (cube.edgesAligned) {
      for (i <- 1 to l1Max) {
        // enter next level
        if (cube.cornersAligned) {
          // flip face centers
          (cube.faceOrientation zip centerFlips).map { x => if (x._1._1 == -1) cube > x._2 }
          // final solution found, if all faces are aligned
          if (cube.facesAligned) {
            println(new Date() + "  (it=" + it + ")")
            println(cube)
            System.exit(1)
          }
        }
        it += 1
        // apply random move from testMoves2
        cube > testMoves2(r.nextInt(testMoves2.size))
      }
    }
  }
}

/**
 *  Rubics Cube
 *
 * <pre>
 *         Facet Index
 *
 *           0  1  2
 *           3  4  5
 *           6  7  8
 *  9 10 11 12 13 14 15 16 17
 * 18 19 20 21 22 23 24 25 26
 * 27 28 29 30 31 32 33 34 35
 *          36 37 38
 *          39 40 41
 *          42 43 44
 *          45 46 47
 *          48 49 50
 *          51 52 53
 * </pre>
 */
object Cube {
  val s2o = Map("|" -> (0, 2), "-" -> (1, 2), "^" -> (0, 4), ">" -> (1, 4), "v" -> (2, 4), "<" -> (3, 4)) withDefault { x => (0, 1) }
  val o2s = s2o map (_.swap) withDefault { x => "?" }

  case class Facet(s: String, var o: (Int, Int)) {
    def rotate(n: Int) = { o = ((o._1 + n) % o._2, o._2); this }
    override def toString = s + o2s(o)
  }

  case class Move(p: Perm, co: Seq[Int]) {
    def *(m: Move) = Move(p * m.p, (co zip m.co) map { x => (x._1 + x._2) % 4 })
  }

  implicit def apply(s: String): Cube = new Cube(s.trim.split("\\s+") map { fs => new Facet(fs.dropRight(1), s2o(fs.last.toString)) })
  def apply(): Cube = """
         B^ B^ B^
         B^ B^ B^
         B^ B^ B^
L^ L^ L^ U^ U^ U^ R^ R^ R^
L^ L^ L^ U^ U^ U^ R^ R^ R^
L^ L^ L^ U^ U^ U^ R^ R^ R^
         F^ F^ F^
         F^ F^ F^
         F^ F^ F^
         D^ D^ D^
         D^ D^ D^
         D^ D^ D^
    """

  val centerFacets = Seq(4, 19, 22, 25, 40, 49)

  val cornerFacets = Seq(
    Seq(0, 2, 8, 6),
    Seq(9, 11, 29, 27),
    Seq(12, 14, 32, 30),
    Seq(15, 17, 35, 33),
    Seq(36, 38, 44, 42),
    Seq(45, 47, 53, 51))

  val edgeFacets = Seq(
    Seq(1, 5, 7, 3),
    Seq(10, 20, 28, 18),
    Seq(13, 23, 31, 21),
    Seq(16, 26, 34, 24),
    Seq(37, 41, 43, 39),
    Seq(46, 50, 52, 48))

  val frMap = Seq(
    0, 0, 1, 3, 0, 1, 3, 2, 2,
    0, 0, 1, 0, 0, 1, 0, 0, 1,
    3, 0, 1, 3, 0, 1, 3, 0, 1,
    3, 2, 2, 3, 2, 2, 3, 2, 2,
    0, 0, 1, 3, 0, 1, 3, 2, 2,
    0, 0, 1, 3, 0, 1, 3, 2, 2)

  def facetRotation(i: Int, j: Int) = (4 + frMap(j) - frMap(i)) % 4

  val B = Move(Perm(Seq(0, 2, 8, 6), Seq(51, 17, 14, 11), Seq(9, 53, 15, 12), Seq(1, 5, 7, 3), Seq(52, 16, 13, 10)), Seq(1, 0, 0, 0, 0, 0))
  val L = Move(Perm(Seq(9, 11, 29, 27), Seq(12, 36, 45, 0), Seq(6, 30, 42, 51), Seq(10, 20, 28, 18), Seq(3, 21, 39, 48)), Seq(0, 1, 0, 0, 0, 0))
  val U = Move(Perm(Seq(12, 14, 32, 30), Seq(11, 8, 33, 36), Seq(6, 15, 38, 29), Seq(13, 23, 31, 21), Seq(7, 24, 37, 20)), Seq(0, 0, 1, 0, 0, 0))
  val R = Move(Perm(Seq(15, 17, 35, 33), Seq(14, 2, 47, 38), Seq(8, 53, 44, 32), Seq(16, 26, 34, 24), Seq(5, 50, 41, 23)), Seq(0, 0, 0, 1, 0, 0))
  val F = Move(Perm(Seq(36, 38, 44, 42), Seq(30, 33, 47, 27), Seq(29, 32, 35, 45), Seq(37, 41, 43, 39), Seq(31, 34, 46, 28)), Seq(0, 0, 0, 0, 1, 0))
  val D = Move(Perm(Seq(45, 47, 53, 51), Seq(27, 44, 17, 0), Seq(42, 35, 2, 9), Seq(46, 50, 52, 48), Seq(43, 26, 1, 18)), Seq(0, 0, 0, 0, 0, 1))

  val simpleMoves = Map(
    "B" -> B, "B2" -> B * B, "B'" -> B * B * B,
    "L" -> L, "L2" -> L * L, "L'" -> L * L * L,
    "U" -> U, "U2" -> U * U, "U'" -> U * U * U,
    "R" -> R, "R2" -> R * R, "R'" -> R * R * R,
    "F" -> F, "F2" -> F * F, "F'" -> F * F * F,
    "D" -> D, "D2" -> D * D, "D'" -> D * D * D)
    .withDefault { x => Move(Perm(), List(0, 0, 0, 0, 0, 0)) }

  implicit def compile(s: String): Move = s.split("[\\s,]+").foldLeft(Move(Perm(), List(0, 0, 0, 0, 0, 0)))((m, ms) => m * simpleMoves(ms))
}

case class Cube(facets: Array[Cube.Facet]) {
  import Cube._

  /**
   * <pre>
   *        (0,1) (0,2) (1,2) (0,4) (1,4) (2,4) (3,4) (-,-)
   *  (0,1) (0,1) (0,2) (1,2) (0,4) (1,4) (2,4) (3,4) (-,-)
   *  (0,2) (0,2) (0,2) (-,-) (0,4) (-,-) (2,4) (-,-) (-,-)
   *  (1,2) (1,2) (-,-) (1,2) (-,-) (1,4) (-,-) (3,4) (-,-)
   *  (0,4) (0,4) (0,4) (-,-) (0,4) (-,-) (-,-) (-,-) (-,-)
   *  (1,4) (1,4) (-,-) (1,4) (-,-) (1,4) (-,-) (-,-) (-,-)
   *  (2,4) (2,4) (2,4) (-,-) (-,-) (-,-) (2,4) (-,-) (-,-)
   *  (3,4) (3,4) (-,-) (3,4) (-,-) (-,-) (-,-) (3,4) (-,-)
   *  (-,-) (-,-) (-,-) (-,-) (-,-) (-,-) (-,-) (-,-) (-,-)
   *  </pre>
   */
  def or2(a: (Int, Int), b: (Int, Int)): (Int, Int) = a match {
    case (0, 1)                                 => b
    case _ if (b == (0, 1))                     => a
    case _ if (a == b)                          => a
    case (0, 2) if (b == (0, 4) || b == (2, 4)) => b
    case (1, 2) if (b == (1, 4) || b == (3, 4)) => b
    case (0, 4) if (b == (0, 2))                => a
    case (1, 4) if (b == (1, 2))                => a
    case (2, 4) if (b == (0, 2))                => a
    case (3, 4) if (b == (1, 2))                => a
    case _                                      => (-1, -1)
  }
  def or2t(a: ((Int, Int), (Int, Int))) = or2(a._1, a._2)
  def or(fs: Seq[Int]) = fs.map { fi => facets(fi).o }.foldLeft((0, 1))(or2)

  def isAligned(x: Seq[(Int, Int)]) = x.filter(p => p._1 != -1).size == 6
  def centerOrientation = centerFacets.map(facets).map(_.o)
  def cornerOrientation = cornerFacets.map(or)
  def cornersAligned = isAligned(cornerOrientation)
  def edgeOrientation = edgeFacets.map(or)
  def edgesAligned = isAligned(edgeOrientation)
  def faceOrientation = ((centerOrientation zip cornerOrientation).map(or2t) zip edgeOrientation).map(or2t)
  def facesAligned = isAligned(faceOrientation)

  def >(m: Move) = {
    m.p(facets)((i, j, facet) => facet.rotate(facetRotation(i, j)))
    (centerFacets zip m.co).foreach { x => facets(x._1).rotate(x._2) }
    this
  }
  def >(m: String): Cube = >(Cube.compile(m))

  override def toString = {
    val f = facets
    s"""
         ${f(0)} ${f(1)} ${f(2)}
         ${f(3)} ${f(4)} ${f(5)}
         ${f(6)} ${f(7)} ${f(8)}
${f(9)} ${f(10)} ${f(11)} ${f(12)} ${f(13)} ${f(14)} ${f(15)} ${f(16)} ${f(17)}
${f(18)} ${f(19)} ${f(20)} ${f(21)} ${f(22)} ${f(23)} ${f(24)} ${f(25)} ${f(26)}
${f(27)} ${f(28)} ${f(29)} ${f(30)} ${f(31)} ${f(32)} ${f(33)} ${f(34)} ${f(35)}
         ${f(36)} ${f(37)} ${f(38)}
         ${f(39)} ${f(40)} ${f(41)}
         ${f(42)} ${f(43)} ${f(44)}
         ${f(45)} ${f(46)} ${f(47)}
         ${f(48)} ${f(49)} ${f(50)}
         ${f(51)} ${f(52)} ${f(53)}
"""
  }
}

class Perm(val p: Seq[(Int, Int)]) {
  type Transition[T] = (Int, Int, T) => T

  val pp = p.toMap
  val ppi = pp.map(_.swap)

  def apply(i: Int) = if (pp.contains(i)) pp(i) else i

  def apply[T](x: Array[T])(implicit f: Transition[T]) = {
    p.map(i => (i._1, i._2, x(i._1))).foreach(j => x.update(j._2, f(j._1, j._2, j._3)))
  }

  def *(p1: Perm) = {
    val p2 = p.map(ij => (ij._1, p1(ij._2)))
    val p3 = p1.pp.filter(x => !ppi.contains(x._1))
    new Perm((p2 ++ p3).filter(p => p._1 != p._2))
  }

  override def toString() = p.foldLeft("Perm(")((a, t) => a + t) + ")"
}

object Perm {
  def apply(cs: Seq[Int]*) = new Perm(cs.flatten zip cs.map { x => x.tail :+ x.head }.flatten)
  implicit def identityTransition[T](i: Int, j: Int, t: T): T = t
}
