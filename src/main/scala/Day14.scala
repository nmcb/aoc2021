import scala.io._

object Day14 extends App:
  val start = System.currentTimeMillis

  val template: String =
    Source
      .fromFile("src/resources/input14.txt")
      .getLines
      .toList
      .head
      .trim

  type Pairs  = Map[(Char, Char), Long]
  type Rules  = Map[(Char, Char), Char]
  type Counts = Map[Char, Long]
  object Counts:
    def empty = Map.empty[Char, Long]

  case class Poly(upairs: Pairs, ucounts: Counts, urules: Rules):
    def max = ucounts.values.max
    def min = ucounts.values.min

    def next: Poly =
      val npairs: Pairs =
        upairs
          .toList
          .flatMap({
            case (pair@(a,b),count) =>
              val c = urules(pair)
              List((a, c) -> count, (c, b) -> count)
          }).groupMapReduce(_._1)(_._2)(_+_)

      val ncounts: Counts =
        upairs
          .foldLeft(ucounts)({
            case (acc0, (pair@(a, b), count)) =>
              val c = urules(pair)
              acc0 + (c -> (acc0.getOrElse(c, 0L) + count))
          }).groupMapReduce(_._1)(_._2)(_+_) 

      Poly(npairs, ncounts, urules)

  val lpairs: Pairs =
    template
      .zip(template.tail)
      .groupMapReduce(identity)(_ => 1L)(_+_)
      
  val lrules: Rules =
    Source
      .fromFile("src/resources/input14.txt")
      .getLines
      .toList
      .drop(2)
      .map(_.trim.split("->").map(_.trim))
      .map(l => (l(0)(0), l(0)(1)) -> l(1).head)
      .toMap

  val lcounts: Counts =
    template.groupMapReduce(identity)(_ => 1L)(_ + _)

  val poly = Poly(lpairs, lcounts, lrules)

  println(s"[0] ${poly.ucounts}")
  println(s"pairs: ${poly.upairs}")
  val result = (1 to 40).foldLeft(poly)((p,i) =>
    val next = p.next
    println(s"[$i] ${next.ucounts}")
    println(s"pairs: ${next.upairs}")
    next
  )
  println(result.max - result.min)
    
  // println(s"Answer 2 = ${answer2.last._2 - answer2.head._2} [${System.currentTimeMillis - start}ms]")
