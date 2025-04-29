import scala.io._

object Day04 extends App:
  val start = System.currentTimeMillis

  val draws =
    Source
      .fromFile("src/main/resources/input04.txt")
      .getLines
      .toList(0).trim.split(",").map(_.trim.toInt)
      .toList

  val Line =
    """(\d+)\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+)""".r 

  def boards =
    Source
      .fromFile("src/main/resources/input04.txt")
      .getLines
      .drop(2)
      .toList
      .map(_.trim)
      .foldLeft((List.empty[Board[Int]], Board.empty[Int])) {
        case ((bs,b), "") =>
          ((b :: bs), Board.empty)
        case ((bs,b), Line(n0,n1,n2,n3,n4)) =>
          (bs, b.addRow(
            List(n0.toInt,n1.toInt,n2.toInt,n3.toInt,n4.toInt)))
        case ((_,_),_) =>
          sys.error("boom!")
      }._1
      
  case class Board[A](rows: List[List[A]], draws: List[A] = List.empty):
    def addRow(row: List[A]): Board[A] =
      copy(rows = rows :+ row)
    def draw(num: A): Board[A] =
      if (!hasBingo) copy(draws = draws :+ num) else this
    def hasBingo: Boolean =
      hasRowWith(draws) || hasColumnWith(draws)
    def unmarked: List[A] =
      rows.flatten.filterNot(draws.contains)
    def lastDraw: A =
      draws.last
    private def hasRowWith(draws: List[A]): Boolean =
      rows.exists(_.forall(draws.contains))
    private def hasColumnWith(draws: List[A]): Boolean =
      rows.transpose.exists(_.forall(draws.contains))

  object Board:
    def empty[A]: Board[A] =
      Board[A](List.empty)

  def playWhoWinsFirst[A](draws: List[A], boards: List[Board[A]] = boards): Board[A] =
    val round = boards.map(_.draw(draws.head))
    round
      .filter(_.hasBingo)
      .headOption
      .getOrElse(playWhoWinsFirst(draws.tail, round))

  val answer1 =
    playWhoWinsFirst(draws)

  println(s"Answer 1 = ${answer1.unmarked.sum * answer1.lastDraw} [${System.currentTimeMillis - start}ms]")

  def playWhoWinsLast[A](draws: List[A], game: List[Board[A]] = boards): Board[A] =
    val round = game.map(_.draw(draws.head))
    val todo = round.filterNot(_.hasBingo)
    if (todo.size > 0)
      playWhoWinsLast(draws.tail, todo)
    else 
      round.filter(_.lastDraw == draws.head).head

  def answer2 =
    playWhoWinsLast(draws)

  println(s"Answer 2 = ${answer2.unmarked.sum * answer2.lastDraw} [${System.currentTimeMillis - start}ms]")
