import scala.annotation.*
import scala.io.*

object Day03 extends App:

  val day: String =
    getClass.getSimpleName.filter(_.isDigit).mkString("")

  val Zero = '0'
  val One  = '1'

  val diagnostics =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map(_.toList)
      .toList

  def mostCommon(l: List[Char]): Char =
    if l.count(_ == Zero) > l.count(_ == One) then Zero else One

  def leastCommon(l: List[Char]): Char =
    if l.count(_ == Zero) > l.count(_ == One) then One else Zero

  val start1  = System.currentTimeMillis

  val gamma   = Integer.parseInt(diagnostics.transpose.map(mostCommon).mkString, 2)
  val epsilon = Integer.parseInt(diagnostics.transpose.map(leastCommon).mkString, 2)
  val answer1 = gamma * epsilon
  
  println(s"Day$day answer 1 = ${answer1} [${System.currentTimeMillis - start1}ms]")


  def filter(bits: List[List[Char]], commonOf: List[Char] => Char): Int =
    def find(todo: List[List[Char]], idx: Int = 0): Int =
      if todo.length == 1 then
        Integer.parseInt(todo.head.mkString, 2)
      else
        val select = commonOf(todo.transpose.apply(idx))
        val next   = todo.filter(bit => bit(idx) == select)
        find(next, idx + 1)
    find(bits)

  val start2 = System.currentTimeMillis

  val oxygenRating       = filter(diagnostics, mostCommon)
  val co2SchrubberRating = filter(diagnostics, leastCommon)
  val answer2            = oxygenRating * co2SchrubberRating

  println(s"Day$day answer 2 = ${answer2} [${System.currentTimeMillis - start2}ms]")
