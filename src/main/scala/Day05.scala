import scala.io._

object Day05 extends App:
  val start = System.currentTimeMillis

  val LineLit =
    """(\d+),(\d+) -> (\d+),(\d+)""".r 

  def lines =
    Source
      .fromFile("src/resources/input05.txt")
      .getLines
      .toList
      .map(_.trim)
      .map {
        case LineLit(x0, y0, x1, y1) =>
          Line(Coord(x0.toInt, y0.toInt), Coord(x1.toInt, y1.toInt))
      }
        
  case class Coord(x: Int, y: Int)

  case class Line(start: Coord, end: Coord):
    val maxX: Int =
      List(start.x, end.x).max
    val maxY: Int =
      List(start.y, end.y).max
    val minX: Int =
      List(start.x, end.x).min
    val minY: Int =
      List(start.y, end.y).min
    val isHorizontal: Boolean =
      start.y == end.y
    val isVertical: Boolean =
      start.x == end.x

  case class Floor(lines: Map[Coord, Int]):
    val crossings: Int =
      lines.filter((coord,count) => count >= 2).size

  object Floor:
    def toCoords(l: Line): Set[Coord] =
      if (l.isHorizontal)
        if (l.end.x >= l.start.x)
          (l.start.x to l.end.x).map(Coord(_, l.start.y)).toSet
        else 
          (l.end.x to l.start.x).map(Coord(_, l.start.y)).toSet
      else if (l.isVertical)
        if (l.end.y >= l.start.y)
          (l.start.y to l.end.y).map(Coord(l.start.x, _)).toSet
        else
          (l.end.y to l.start.y).map(Coord(l.start.x, _)).toSet
      else
        (0 to (l.maxX - l.minX)).map(n =>
          if (l.end.x > l.start.x)
            if (l.end.y > l.start.y)
              Coord(l.start.x + n, l.start.y + n)
            else
              Coord(l.start.x + n, l.start.y - n)
          else
            if (l.end.y > l.start.y)
              Coord(l.start.x - n, l.start.y + n)
            else
              Coord(l.start.x - n, l.start.y - n)
        ).toSet

    def apply(lines: List[Line]): Floor =
      Floor(lines.foldLeft(Map.empty[Coord,Int])((acc,line) =>
        val coords = toCoords(line)
        val added  = (coords diff acc.keySet).map(c => c -> 1)
        val update = (acc.keySet intersect coords).map(c => c -> (acc(c) + 1))
        val result = acc ++ added ++ update
        result
        ))

  val answer =
    Floor(lines).crossings

  println(s"Answer = ${answer} [${System.currentTimeMillis - start}ms]")
