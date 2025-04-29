import scala.io._
import scala.collection.immutable.HashSet

object Day05 extends App:
  val start = System.currentTimeMillis

  val LineLit =
    """(\d+),(\d+) -> (\d+),(\d+)""".r 

  val lines =
    Source
      .fromFile("src/main/resources/input05.txt")
      .getLines
      .toList
      .map(_.trim)
      .map {
        case LineLit(x0, y0, x1, y1) if y0 == y1 =>
          HLine(x0.toInt, y0.toInt, x1.toInt, y1.toInt)
        case LineLit(x0, y0, x1, y1) if x0 == x1 =>
          VLine(x0.toInt, y0.toInt, x1.toInt, y1.toInt)
        case LineLit(x0, y0, x1, y1) =>
          DLine(x0.toInt, y0.toInt, x1.toInt, y1.toInt)
      }

  sealed trait Line:
    val points: Set[(Int,Int)]

  case class HLine(x0: Int, y0: Int, x1: Int, y1: Int)
    extends Line:
    val points: Set[(Int,Int)] =
      if (x0 < x1)
        (x0 to x1).map((_, y0)).toSet
      else
        (x1 to x0).map((_, y0)).toSet
  case class VLine(x0: Int, y0: Int, x1: Int, y1: Int)
    extends Line:
    val points: Set[(Int,Int)] =
      if (y0 < y1)
        (y0 to y1).map((x0, _)).toSet
      else
        (y1 to y0).map((x0, _)).toSet
  case class DLine(x0: Int, y0: Int, x1: Int, y1: Int)
    extends Line:

    val points: Set[(Int,Int)] =
      (0 to (math.max(x0, x1) - math.min(x0, x1))).map( n =>
        if (x0 < x1)
          if (y0 < y1)
            (x0 + n, y0 + n)
          else
            (x0 + n, y0 - n)
        else
          if (y0 < y1)
            (x0 - n, y0 + n)
          else
            (x0 - n, y0 - n)
      ).toSet

  case class Floor(locations: Map[(Int,Int), Int]):
    val crossings: Int =
      locations.filter((_,count) => count >= 2).size

  object Floor:
    def apply(lines: List[Line]): Floor =
      Floor(lines.foldLeft(Map.empty[(Int,Int),Int])((a,l) =>
        val added  = (l.points diff a.keySet).map(p => p -> 1)
        val update = (a.keySet intersect l.points).map(p => p -> (a(p) + 1))
        a ++ added ++ update
      ))

  val answer = Floor(lines).crossings

  println(s"Answer = ${answer} [${System.currentTimeMillis - start}ms]")
