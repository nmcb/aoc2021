import scala.io._

object Day03 extends App:
  val start = System.currentTimeMillis

  val Zero = '0'
  val One  = '1'

  val diagnostics =
    Source
      .fromFile("src/resources/input03.txt")
      .getLines
      .map(_.toList)
      .toList

  def mostCommon(l: List[Char]): Char =
    val zeros = l.count(_ == Zero)
    val ones  = l.count(_ == One)
    if (zeros > ones) Zero else One

  def leastCommon(l: List[Char]): Char =
    val zeros = l.count(_ == Zero)
    val ones  = l.count(_ == One)
    if (zeros > ones) One else Zero

  val gamma   = Integer.parseInt(diagnostics.transpose.map(mostCommon).mkString, 2)
  val epsilon = Integer.parseInt(diagnostics.transpose.map(leastCommon).mkString, 2)
  
  println(s"Answer 1 = ${gamma * epsilon} [${System.currentTimeMillis - start}ms]")


  def find(bits: List[List[Char]], idx: Int = 0)(commonOf: List[Char] => Char): Int =
    val next =
      bits.filter(bit => bit(idx) == commonOf(diagnostics.transpose.apply(idx)))
    if (next.length == 1)
      Integer.parseInt(next.head.mkString, 2)
    else
      find(next, idx + 1)(commonOf)

  val oxygenRating       = find(diagnostics)(mostCommon)
  val co2SchrubberRating = find(diagnostics)(leastCommon)

  println(s"Answer 2 = ${oxygenRating * co2SchrubberRating} [${System.currentTimeMillis - start}ms]")
