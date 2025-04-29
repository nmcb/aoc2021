import scala.io._
import scala.quoted.Expr


object Day11 extends App:
  val start = System.currentTimeMillis

  val octos =
    Octopuses(Source
      .fromFile("src/main/resources/input11.txt")
      .getLines
      .map(_.map(_.toString.toInt).toList)
      .zipWithIndex.flatMap((row, y) =>
        row.zipWithIndex.map((level, x) =>
          (x,y) -> level
    )).toMap)

  case class Octopuses(energies: Map[(Int,Int),Int]):
    val maxX = energies.keySet.map(_._1).max
    val maxY = energies.keySet.map(_._2).max

    def asMatrix: List[List[Int]] =
      (0 to maxY).toList.foldLeft(List.empty)((acc,yi) =>
        acc :+ (0 to maxX).toList.foldLeft(List.empty)((row,xi) =>
          row :+ energies.get((xi,yi)).get
      ))  

    override def toString: String =
      asMatrix.foldLeft("")((str,rows) => str + rows.mkString("") + "\n")

    def neighbours(x: Int, y: Int): List[(Int,Int)] =
      List((-1,-1),(0,-1),(1,-1), (-1,0),(1,0), (-1,1),(0,1),(1,1))
        .filter((nx, ny) => energies.keySet.contains((x + nx, y + ny))) 
        .map((nx, ny) => (x + nx, y + ny))

    private def loop(flashes: List[(Int,Int)], acc: Map[(Int,Int),Int], flashed: List[(Int,Int)] = List.empty): (Map[(Int,Int),Int], Int) =
      if (flashes.isEmpty)
        // println(s"no more to flash")
        (acc, flashed.size)
      else if (flashed.contains(flashes.head))
        // val (x,y)     = flashes.head
        // println(s"($x,$y) already flashed")
        loop(flashes.tail, acc, flashed)
      else
        val (x,y)     = flashes.head
        // println(s"($x,$y) propagating to neighbours : ${neighbours(x,y)}")
        val update    = neighbours(x,y).map(octo => octo -> (acc.get(octo).get + 1)).toMap
        // println(s"($x,$y) update : $update")
        val propagate = update.filter((_,level) => level > 9).keySet.toList
        // println(s"($x,$y) propagate : $propagate")
        loop(propagate ++ flashes.tail, acc ++ update, ((x,y)) :: flashed)

    private def normalise: Octopuses =
      Octopuses(energies.map((o,l) => if (l > 9) (o -> 0) else (o -> l)))

    def step: (Octopuses, Int) =
      val updated = energies.map((o,l) => o -> (l + 1))
      val flashes = updated.filter((_,l) => l > 9).keySet.toList.sorted
      val (propagated, flashed) = loop(flashes, updated)
      (Octopuses(propagated).normalise, flashed)

  println(s"Before any steps:")
  println(octos)

  val answer1 =
    (1 to 100).foldLeft((octos, 0)){ case ((os,res),ix) =>
      val (updated, flashed) = os.step
      println(s"After step : $ix")
      println(updated)
      (updated, res + flashed)
    }._2

  println(s"Answer 1 = ${answer1} [${System.currentTimeMillis - start}ms]")

  val answer2 =
    (1 to 10000).foldLeft((octos, 0)){ case ((os,res),ix) =>
      val (updated, flashed) = os.step
      if (flashed == 100) throw new Exception(s"Answer 2 : $ix")
      println(s"After step : $ix")
      println(updated)
      (updated, res + flashed)
    }._2

  println(s"Answer 1 = ${answer1} [${System.currentTimeMillis - start}ms]")
