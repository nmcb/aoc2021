import scala.io._
import scala.collection.*
import scala.util.*

object Day24B extends App:
  val timestamp = System.currentTimeMillis

  case class Step(a: Int, b: Int, d: Int)

  case class Rel(delta: Int, a: Int, b: Int):
    def abs: Rel =
      if (delta >= 0) this else Rel(-delta, b, a)

  val constraints = List(
    Rel( 6, 0,13),
    Rel(-7, 1,12),
    Rel(-5, 4,11),
    Rel( 3, 5,10),
    Rel( 2, 8, 9),
    Rel(-1, 6, 7),
    Rel( 0, 2, 3)
  )

  val solve: Seq[Range] =

    def go(n: Int, constraints: Seq[Rel]): Seq[Range] =
      val digits = mutable.IndexedSeq.fill(n)(1 to 9)

      for (Rel(delta, i, j) <- constraints.map(_.abs))
        digits(i) = digits(i).min to (9 - delta)
        digits(j) = (1 + delta) to digits(j).max
      digits.toSeq

    go(14, constraints)

  val max: String =
    solve.map(_.max).mkString

  val min: String =
    solve.map(_.min).mkString

  println(s"answer 1: $max [${System.currentTimeMillis - timestamp}ms]")
  assert(max == "39999698799429")

  println(s"answer 2: $min [${System.currentTimeMillis - timestamp}ms]")
  assert(min == "18116121134117")
  