import scala.io._

object Day05 extends App:
  val start   = System.currentTimeMillis
  val LineLit = """(\d+),(\d+) -> (\d+),(\d+)""".r 

  def lines: Set[Line] =
    Source
      .fromFile("src/resources/input05.txt")
      .getLines
      .map(_.trim)
      .map { case LineLit(x0, y0, x1, y1) => Line(x0.toInt, y0.toInt, x1.toInt, y1.toInt) }
      .toSet
      
  case class Line(x0: Int, y0: Int, x1: Int, y1: Int):
    val maxX: Int = math.max(x0, x1)
    val maxY: Int = math.max(y0, y1)
    val minX: Int = math.min(x0, x1)
    val minY: Int = math.min(y0, y1)
    val deltaX: Int = if (x0 < x1) 1 else if (x0 > x1) -1 else 0
    val deltaY: Int = if (y0 < y1) 1 else if (y0 > y1) -1 else 0
    val steps: Range = (0 to math.max(maxX - minX, maxY - minY))
    val points: Set[(Int,Int)] = steps.map(n => (x0 + (n * deltaX), y0 + (n * deltaY))).toSet
    def on(x: Int, y: Int): Boolean = steps.exists(n => (x == x0 + (n * deltaX)) && (y == y0 + (n * deltaY)))

  case class Floor(lines: Set[Line]):
    val crossings: Int = lines.flatMap(l => l.points).count((x, y) => lines.count(_.on(x, y)) >= 2)

  val answer = Floor(lines).crossings

  println(s"Answer = ${answer} [${System.currentTimeMillis - start}ms]")
  assert(answer == 20271)
