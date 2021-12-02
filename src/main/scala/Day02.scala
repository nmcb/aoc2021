import scala.io._

object Day02 extends App:
  val start = System.currentTimeMillis

  sealed abstract class Inst(delta: Long)
  case class Up(delta: Long)      extends Inst(delta)
  case class Down(delta: Long)    extends Inst(delta)
  case class Forward(delta: Long) extends Inst(delta)

  object Inst:
    def fromLine(l: String): Inst =
      l.split(" ").toList match
        case command :: d :: Nil if command.startsWith("up")      => Up(d.toLong)
        case command :: d :: Nil if command.startsWith("down")    => Down(d.toLong)
        case command :: d :: Nil if command.startsWith("forward") => Forward(d.toLong)
        case _ => sys.error(s"error parsing line: $l")

  case class Sub(heading: Long = 0, depth: Long = 0, aim: Long = 0):
    def move(is: List[Inst]): Sub =
      is.foldLeft(this)((cur,is) => is match
        case Up(d) =>
          cur.copy( aim = cur.aim - d)
        case Down(d) =>
          cur.copy( aim = cur.aim + d)
        case Forward(d) =>
          cur.copy( heading = cur.heading + d, depth = cur.depth + (cur.aim * d)))

  object Sub {
    def init: Sub = Sub()
  }

  val instructions: List[Inst] =
    Source
      .fromFile("src/resources/input02.txt")
      .getLines
      .map(Inst.fromLine)
      .toList

  val answer: Long =
    val sub = Sub.init.move(instructions)
    sub.heading * sub.depth
  
  println(s"Answer: ${answer} [${System.currentTimeMillis - start}ms]")
