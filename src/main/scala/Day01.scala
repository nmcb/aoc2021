import scala.io._

object Day01 extends App {

  val start1 = System.currentTimeMillis

  val answer1: Int =
      Source
        .fromFile("src/resources/input01.txt")
        .getLines
        .map(_.trim.toInt)
        .sum

  println(s"Answer part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

}