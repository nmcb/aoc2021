import scala.io._

object Day01 extends App {

  val start1 = System.currentTimeMillis

  val depths: List[Int] =
    Source
      .fromFile("src/resources/input01.txt")
      .getLines
      .map(_.trim.toInt)
      .toList
  
  def incs(l: List[Int], n: Int = 0, p: Int = 0): Int =
    if (l.isEmpty)
      n - 1
    else if (l.head > p)
      incs(l.tail, n + 1, l.head)
    else
      incs(l.tail, n, l.head)

  println(s"Answer part 1: ${incs(depths)} [${System.currentTimeMillis - start1}ms]")

  val answer2: List[Int] =
    depths
      .sliding(3)
      .map(_.sum)
      .toList

  println(s"Answer part 2: ${incs(answer2)} [${System.currentTimeMillis - start1}ms]")

}