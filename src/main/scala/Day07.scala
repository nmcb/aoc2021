import scala.io._

object Day07 extends App:
  val start = System.currentTimeMillis

  val positions: List[Int] =
    Source
      .fromFile("src/main/resources/input07.txt")
      .getLines
      .toList
      .flatMap(_.split(",").map(_.trim.toInt))

  def fuelConsumptionTo(target: Int): Int =
    positions.foldLeft(0)((acc,pos) =>
      val steps = math.abs(target - pos)
      val total = (1 to steps).foldLeft(0)((fuel,step) => (fuel + step))
      acc + total)

  val answer =
    println(s"Calculating fuel consumption for ${positions.max} position targets\n")
    (1 to positions.max).map(pos => {
      val fuel = fuelConsumptionTo(pos)
      println(s"\u001b[FTarget position $pos consumes $fuel fuel")
      fuel
    }).min

  println(s"Answer = ${answer} [${System.currentTimeMillis - start}ms]")
  assert(answer == 91257582)
