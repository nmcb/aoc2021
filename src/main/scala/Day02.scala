import scala.io._

object Day02 extends App {

  val start1 = System.currentTimeMillis

  enum Dir(delta: Int):
    case Up(delta: Int)      extends Dir(delta)
    case Down(delta: Int)    extends Dir(delta)
    case Forward(delta: Int) extends Dir(delta)

  import Dir._

  case class Loc(heading: Int = 0, depth: Int = 0, aim: Int = 0)

  val input =
    Source
      .fromFile("src/resources/input02.txt")
      .getLines
      .map(_.split(" ").toList match {
        case "up"      :: (d: String) :: Nil => Up(d.toInt)
        case "down"    :: (d: String) :: Nil => Down(d.toInt)
        case "forward" :: (d: String) :: Nil => Forward(d.toInt)
        case l => sys.error(s"error parsing line: $l")
      })
      .toList

  def move(is: List[Dir], init: Loc = Loc()): Loc =
    is.foldLeft(init)((cur,is) => is match {
      case Up(d)      => Loc(cur.heading, cur.depth, cur.aim - d)
      case Down(d)    => Loc(cur.heading, cur.depth, cur.aim + d)
      case Forward(d) => Loc(cur.heading + d, cur.depth + (cur.aim * d), cur.aim)
    })

  val answer1: Int =
    val p = move(input)
    p.heading * p.depth
  
  println(s"Answer part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

}