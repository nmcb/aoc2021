import scala.io._

object Day14 extends App:

  val lines: Seq[String] =
    Source
      .fromFile("src/resources/input14.txt")
      .getLines
      .toSeq

  val template: String =
    lines.head.trim

  val rules: Map[(Char, Char), Char] =
    lines
      .drop(2)
      .map( line =>
        val Array(pair,insert) = line.trim.split("->").map(_.trim)
        (pair.charAt(0), pair.charAt(1)) -> insert.charAt(0)
      )
      .toMap

  def fst[A](pair: (A,?)): A =
    pair._1

  def snd[B](pair: (?,B)): B =
    pair._2

  def step(pairs: Map[(Char, Char), Long], counts: Map[Char, Long]): (Map[(Char, Char), Long], Map[Char, Long]) =
    ( pairs.toSeq.flatMap((pair, count) =>
        val c = rules(pair)
        Seq((fst(pair), c) -> count, (c, snd(pair)) -> count)
      ).groupMapReduce(fst)(snd)(_ + _)
    , pairs.foldLeft(counts)((acc, count) =>
        val char = rules(fst(count))
        acc.updated(char, acc.getOrElse(char, 0L) + snd(count))
      ).groupMapReduce(fst)(snd)(_ + _) 
    )

    

  val initPairs: Map[(Char, Char), Long] =
    template
      .zip(template.tail)
      .groupMapReduce(identity)(_ => 1L)(_+_)

  val initCounts: Map[Char, Long] =
    template.groupMapReduce(identity)(_ => 1L)(_ + _)
        
  def max(counts: Map[Char, Long]) =
    counts.values.max
    
  def min(counts: Map[Char, Long]) =
    counts.values.min

  val start: Long =
    System.currentTimeMillis

  val (pairs1, counts1) =
    (1 to 10).foldLeft((initPairs, initCounts)) {
      case ((pairs,counts), index) => step(pairs, counts)
    }

  println(s"Answer 1 = ${max(counts1) - min(counts1)} [${System.currentTimeMillis - start}ms]")

  val (pairs2, counts2) =
    (1 to 30).foldLeft((pairs1, counts1)) {
      case ((pairs,counts), index) => step(pairs, counts)
    }

  println(s"Answer 2 = ${max(counts2) - min(counts2)} [${System.currentTimeMillis - start}ms]")
