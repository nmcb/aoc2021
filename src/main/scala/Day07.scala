import scala.io._
import scala.collection.immutable.HashSet

object Day07 extends App:
  val start = System.currentTimeMillis

  val init: List[Int] =
    Source
      .fromFile("src/resources/input07.txt")
      .getLines
      .toList
      .flatMap(_.split(",").map(_.trim.toInt))

  def consumption(target: Int): Int =
    init.foldLeft(0)((acc,pos) =>
      val steps = math.abs(target - pos)
      val total = (0 to steps).foldLeft(0)((fuel,step) => (fuel + step))
      acc + total)

  val answer =
    val positions = (1 to init.max)
    println(s"Calculating fuel consumption for ${init.max} position targets\n")
    positions.map(pos => {
      val fuel = consumption(pos)
      println(s"\u001b[FPosition $pos consumes $fuel fuel")
      fuel
    }).min

  println(s"Answer = ${answer} [${System.currentTimeMillis - start}ms]")
