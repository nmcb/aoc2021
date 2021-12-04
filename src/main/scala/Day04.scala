import scala.io._

object Day04 extends App:
  val start = System.currentTimeMillis

  val Zero = '0'
  val One  = '1'

  val draws: List[Int] =
    Source
      .fromFile("src/resources/input04.txt")
      .getLines
      .toList(0).trim.split(",").map(_.toInt)
      .toList

  case class Board(rows: List[List[Int]] = List.empty, draws: List[Int] = List.empty):
    def hasRowWith(draws: List[Int]): Boolean =
      rows.filter(_.forall(draws.contains _)).nonEmpty
    def hasColumnWith(draws: List[Int]): Boolean =
      rows.transpose.filter(_.forall(draws.contains _)).nonEmpty
    def draw(num: Int): Board =
      if (!hasWon) copy(draws = draws :+ num) else this
    def hasWon: Boolean =
      hasRowWith(draws) || hasColumnWith(draws)
    def unmarked: List[Int] =
      rows.flatten.filterNot(draws.contains)
    def lastDraw: Int =
      draws.last
    
      
  val boards: List[Board] =
    Source
      .fromFile("src/resources/input04.txt")
      .getLines
      .drop(2)
      .toList
      .map(l => if (l.trim.isEmpty) List.empty else l.trim.split("\\s+").map(_.toInt).toList)
      .foldLeft(List(Board()))((acc,row) =>
        if (row.isEmpty)
          acc :+ Board()
        else
          acc.init :+ Board(acc.last.rows :+ row))
      .init



  def first(draws: List[Int], game: List[Board] = boards): Board =
    val next = game.map(_.draw(draws.head))
    val won  = next.filter(_.hasWon).headOption
    if (won.isDefined)
      won.get
    else
      first(draws.tail, next)

  val answer1  = first(draws)

  println(s"Answer 1 = ${answer1.unmarked.sum * answer1.lastDraw} [${System.currentTimeMillis - start}ms]")

  def last(draws: List[Int], game: List[Board] = boards): Board =
    val next = game.map(_.draw(draws.head))
    val todo = next.filterNot(_.hasWon)
    println("--todo--")
    println(todo.mkString("\n"))
    if (todo.size > 0)
      last(draws.tail, todo)
    else 
      next.filter(_.lastDraw == draws.head).head

  def answer2 = last(draws)

  println(s"Answer 2 = ${answer2.unmarked.sum * answer2.lastDraw} [${System.currentTimeMillis - start}ms]")
