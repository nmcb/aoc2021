import scala.io._

object Day17 extends App:

  
  val start = System.currentTimeMillis
  
  case class Probe(x: Int, y: Int, vx: Int, vy: Int):
    def step: Probe =
      val nx  = x + vx
      val ny  = y + vy
      val nvx = if (vx > 0) vx - 1 else if (vx < 0) vx + 1 else 0
      val nvy = vy - 1
      Probe(nx, ny, nvx, nvy)

  // val targetX =  20 to 30
  // val targetY = -10 to -5
  val targetX =  150 to 193
  val targetY = -136 to -86

  def go(x: Int, y: Int, vx: Int, vy: Int, maxY: Int = 0): Option[Int] =
    Probe(x, y ,vx, vy).step match {
      case Probe(nx, ny, nvx, nvy) if targetX.contains(nx) && targetY.contains(ny) => Some(maxY)
      case Probe(nx, ny, nvx, nvy) if nx > targetX.max => None
      case Probe(nx, ny, nvx, nvy) if ny < targetY.min => None
      case Probe(nx, ny, nvx, nvy) => go(nx, ny, nvx, nvy, if (ny > maxY) ny else maxY)
    }

  val answer1 =
    (0 to 50).flatMap(x => (0 to 200).map(y => (x,y))).map((vx,vy) =>
      go(0, 0, vx, vy)
    ).filter(_.isDefined).map(_.get).max

  println(s"Answer 1 = ${answer1} [${System.currentTimeMillis - start}ms]")

  val answer2 =
    (0 to 200).flatMap(x => (-200 to 200).map(y => (x,y))).map((vx,vy) =>
      go(0, 0, vx, vy)
    ).filter(_.isDefined)

  println(s"Answer 2 = ${answer2.size} [${System.currentTimeMillis - start}ms]")
