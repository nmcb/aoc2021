import scala.io._

object Day13 extends App:
  val start = System.currentTimeMillis

  val dots =
    Source
      .fromFile("src/resources/input13.txt")
      .getLines
      .map(_.trim)
      .takeWhile(_ != "")
      .map(_.split(",").toList)
      .map(xy => (xy(0).toInt,xy(1).toInt))
      .toList
      
  val folds =
    val prefix = "fold along"
    Source
      .fromFile("src/resources/input13.txt")
      .getLines
      .map(_.trim)
      .dropWhile(!_.startsWith(prefix))
      .map(_.drop(prefix.length).trim)
      .map(fold =>
        if      (fold.startsWith("x=")) Fold(fold.drop(2).trim.toInt, "ver")
        else if (fold.startsWith("y=")) Fold(fold.drop(2).trim.toInt, "hor")
        else    sys.error(s"unparsable fold=$fold")
      ).toList

  case class Paper(dots: List[(Int,Int)]):
    val maxX = dots.map(_._1).max 
    val maxY = dots.map(_._2).max

    def count: Int =
      dots.filter((x,y) => (0 to maxX).contains(x) && (0 to maxY).contains(y)).distinct.size

    override def toString: String =
      (0 to maxY).foldLeft("")((a,y) =>
        a + (0 to maxX).foldLeft("")((l,x) =>
          l + (if (dots.contains((x,y))) "#" else ".")
        ) + "\n" 
      )

    def fold(f: Fold): Paper =
      if (f.direction == "hor")
        val next = (0 until f.line).foldLeft(List.empty[(Int,Int)])((a,y) =>
          a ++ (0 to maxX).foldLeft(List.empty[(Int,Int)])((l,x) =>
            val a = dots.find(_ == (x,y))
            val b = dots.find(_ == (x, 2 * f.line - y))
            (a,b) match {
              case (Some(c1), Some(c2)) => l ++ List(c1, (c2._1, 2 * f.line - c2._2))
              case (Some(c1), None)     => l ++ List(c1)
              case (None, Some(c2))     => l ++ List((c2._1, 2 * f.line - c2._2))
              case (None, None)         => l
            }
          )
        )
        Paper(next.distinct)
      else
        val next = (0 until f.line).foldLeft(List.empty[(Int,Int)])((a,x) =>
          a ++ (0 to maxY).foldLeft(List.empty[(Int,Int)])((l,y) =>
            val a = dots.find(_ == (x,y))
            val b = dots.find(_ == (2 * f.line - x, y))
            (a,b) match {
              case (Some(c1), Some(c2)) => l ++ List(c1, (2 * f.line - c2._1, c2._2))
              case (Some(c1), None)     => l ++ List(c1)
              case (None, Some(c2))     => l ++ List((2 * f.line - c2._1, c2._2))
              case (None, None)         => l
            }
          )
        )
        Paper(next.distinct)


  case class Fold(line: Int, direction: String)
  
  println(folds.head)

  val paper = Paper(dots)
  println(paper.maxX)
  println(paper.maxY)
  println()

  val folded = paper.fold(folds.head)
  println(folded.maxX)
  println(folded.maxY)
  println(folded.count)
  println()

  // val folded2 = folded.fold(folds.tail.head)
  // println(folded2.maxX)
  // println(folded2.maxY)
  // println(folded2.count)
  // println(folded2)

  val run =
    folds.foldLeft(paper)((p,f) => p.fold(f))
  println(run)

  // println(s"Answer 1 = ${pathsPart1.size} [${System.currentTimeMillis - start}ms]")
