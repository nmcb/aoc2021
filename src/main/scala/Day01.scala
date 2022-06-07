import scala.io._

object Day01 extends App:

  val start1 = System.currentTimeMillis

  val depths: List[Int] =
    Source
      .fromFile("src/resources/input01.txt")
      .getLines
      .map(_.trim.toInt)
      .toList
  
  def incs(l: List[Int]): Int =
    l.sliding(2).count(l => l(1) > l(0))

  println(s"Answer part 1: ${incs(depths)} [${System.currentTimeMillis - start1}ms]")

  val answer2: List[Int] =
    depths
      .sliding(3)
      .map(_.sum)
      .toList

  println(s"Answer part 2: ${incs(answer2)} [${System.currentTimeMillis - start1}ms]")
