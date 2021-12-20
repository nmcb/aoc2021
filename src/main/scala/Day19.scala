import scala.io.*

object Day19 extends App:
  val start = System.currentTimeMillis

  val input: List[String] =
    Source
      .fromFile("src/resources/input19.txt")
      .getLines
      .toList

  val VecLit =
    """([-+]?\d+),([-+]?\d+),([-+]?\d+)""".r

  type Vec = (Int,Int,Int)

  object Vec:

    def add(b: Vec)(a: Vec): Vec =
      (a._1 + b._1, a._2 + b._2, a._3 + b._3)

    def substract(b: Vec)(a: Vec): Vec =
      (a._1 - b._1, a._2 - b._2, a._3 - b._3)

    def distance(a: Vec)(b: Vec): Int =
      (b._1 - a._1).abs + (b._2 - a._2).abs + (b._3 - a._3).abs

  case class Scanner(idx: Int, report: Set[Vec])

  val scanners: List[Scanner] =
    input
      .foldLeft(List.empty[List[Vec]])((a,l) =>
        if (l.startsWith("--- scanner"))
          List.empty[Vec] :: a
        else if (l.isEmpty)
          a
        else l match
          case VecLit(x,y,z) => ((x.toInt, y.toInt, z.toInt) :: a.head) :: a.tail
          case _ => sys.error(s"boom: $l"))
      .reverse
      .zipWithIndex
      .map((report,index) => Scanner(index, report.toSet))

  def orientations(pos: Vec): Seq[Vec] =
    val (x, y, z) = pos
    Seq(
         (+x,+y,+z),(-y,+x,+z),(-x,-y,+z),(+y,-x,+z)
       , (-x,+y,-z),(+y,+x,-z),(+x,-y,-z),(-y,-x,-z)
       , (-z,+y,+x),(-z,+x,-y),(-z,-y,-x),(-z,-x,+y)
       , (+z,+y,-x),(+z,+x,+y),(+z,-y,+x),(+z,-x,-y)
       , (+x,-z,+y),(-y,-z,+x),(-x,-z,-y),(+y,-z,-x)
       , (+x,+z,-y),(-y,+z,-x),(-x,+z,+y),(+y,+z,+x)
       )

  def find(beacons: Set[Vec], scanner: Scanner): Option[(Set[Vec], Vec)] =
    import Vec.*
    val result =
      for {
        transposed <- scanner.report.map(orientations).transpose.map(_.toSet)
        local      <- beacons
        remote     <- transposed
        position = substract(remote)(local)
        if transposed.map(add(position)).filter(beacons).size >= 10
      } yield (transposed.map(add(position)), position)
    
    result.headOption
  
  def solve(scanners: Seq[Scanner]): (Set[Vec], Set[Vec]) = {

    // todo     -> scanners to match report for
    // known    -> all known beacons in this step
    // found    -> found beacons from last step
    // scanners -> all known scanner positions including scanner 0 
    def go(todo: Seq[Scanner], known: Set[Vec], found: Set[Vec], scanners: Set[Vec]): (Set[Vec], Set[Vec]) =
      println(s"todo=${todo.size}, known=${known.size}, found=${found.size}, scanners=${scanners.size}")
      val beacons = known ++ found
      if (todo.isEmpty)
        (beacons, scanners)
      else
        val result = for {
          scanner                 <- todo
          (transposed, positions) <- find(found, scanner)
        } yield (scanner, transposed, positions)
        val matched   = result.map(_._1)
        val oriented  = result.map(_._2)
        val positions = result.map(_._3)
          
        val next = todo.filterNot(matched.contains)
        val pack = if (oriented.nonEmpty) oriented.reduce(_ ++ _) else Set.empty
        val newScannerPoss = scanners ++ positions
        go(next, beacons, pack, scanners ++ positions)

    go(scanners.tail, Set.empty, scanners.head.report, Set((0,0,0)))
  }

  val (beacons, positions) = solve(scanners) 

  val answer1 = beacons.size
  println(s"answer 1: $answer1 [${System.currentTimeMillis - start}ms]")
  assert(answer1 == 320)

  val answer2 =
    val distances = for {
      a <- positions
      b <- positions
    } yield Vec.distance(b)(a)    
    distances.max

  println(s"answer 2: $answer2 [${System.currentTimeMillis - start}ms]")
  assert(answer2 == 9655)


  

