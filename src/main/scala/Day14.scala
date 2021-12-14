import scala.io._

object Day14 extends App:
  val start = System.currentTimeMillis

  val lines: List[String] =
    Source
      .fromFile("src/resources/input14.txt")
      .getLines
      .toList

  val template: String =
    lines.head.trim

  val rules: Map[(Char, Char), Char] =
    lines.drop(2)
      .map( line =>
        val Array(pair,insert) = line.trim.split("->").map(_.trim)
        (pair(0), pair(1)) -> insert.head
      )
      .toMap

  def next(pairs: Map[(Char, Char), Long], counts: Map[Char, Long]): (Map[(Char, Char), Long], Map[Char, Long]) =
    val npairs: Map[(Char, Char), Long] =
      pairs.toList.flatMap({ case (pair@(a, b), count) =>
        val c = rules(pair)
        List((a, c) -> count, (c, b) -> count)
      }).groupMapReduce(_._1)(_._2)(_+_)

    val ncounts: Map[Char, Long] =
      pairs.foldLeft(counts)({ case (acc, (pair@(a, b), count)) =>
        val char = rules(pair)
        acc.updated(char, acc.getOrElse(char, 0L) + count)
      }).groupMapReduce(_._1)(_._2)(_+_) 

    (npairs, ncounts)

  def max(counts: Map[Char, Long]) =
    counts.values.max
    
  def min(counts: Map[Char, Long]) =
    counts.values.min

  val initPairs: Map[(Char, Char), Long] =
    template
      .zip(template.tail)
      .groupMapReduce(identity)(_ => 1L)(_+_)

  val initCounts: Map[Char, Long] =
    template.groupMapReduce(identity)(_ => 1L)(_ + _)
        
  val (pairs, counts) =
    (1 to 40).foldLeft((initPairs, initCounts))({
      case ((pairs,counts), index) => next(pairs, counts)
    })

  println(max(counts) - min(counts))
