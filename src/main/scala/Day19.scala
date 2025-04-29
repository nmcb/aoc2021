import scala.io.*

object Day19 extends App:
  val start = System.currentTimeMillis

  val input: List[String] =
    Source
      .fromFile("src/main/resources/input19.txt")
      .getLines
      .toList

  import vector.*

  case class Scanner(idx: Int, report: Set[Vec3])

  val scanners: List[Scanner] =
    input
      .foldLeft(List.empty[List[Vec3]])((a,l) =>
        if (l.startsWith("--- scanner"))
          List.empty[Vec3] :: a
        else if (l.isEmpty)
          a
        else
          (Vec3.parse(l) :: a.head) :: a.tail
      )
      .reverse
      .zipWithIndex
      .map((report,index) => Scanner(index, report.toSet))

  def orientations(pos: Vec3): Seq[Vec3] =
    val Vec3(x, y, z) = pos
    Seq(
         Vec3(+x,+y,+z), Vec3(-y,+x,+z), Vec3(-x,-y,+z), Vec3(+y,-x,+z)
       , Vec3(-x,+y,-z), Vec3(+y,+x,-z), Vec3(+x,-y,-z), Vec3(-y,-x,-z)
       , Vec3(-z,+y,+x), Vec3(-z,+x,-y), Vec3(-z,-y,-x), Vec3(-z,-x,+y)
       , Vec3(+z,+y,-x), Vec3(+z,+x,+y), Vec3(+z,-y,+x), Vec3(+z,-x,-y)
       , Vec3(+x,-z,+y), Vec3(-y,-z,+x), Vec3(-x,-z,-y), Vec3(+y,-z,-x)
       , Vec3(+x,+z,-y), Vec3(-y,+z,-x), Vec3(-x,+z,+y), Vec3(+y,+z,+x)
       )

  def find(beacons: Set[Vec3], scanner: Scanner): Option[(Set[Vec3], Vec3)] =
    val result =
      for {
        transposed <- scanner.report.map(orientations).transpose.map(_.toSet)
        local      <- beacons
        remote     <- transposed
        position = remote - local
        if transposed.map(position + _).filter(beacons).size >= 10
      } yield (transposed.map(position + _), position)
    
    result.headOption
  
  def solve(scanners: Seq[Scanner]): (Set[Vec3], Set[Vec3]) = {

    // todo     -> scanners to match report for
    // known    -> all known beacons in this step
    // found    -> found beacons from last step
    // scanners -> all known scanner positions including scanner 0 
    def go(todo: Seq[Scanner], known: Set[Vec3], found: Set[Vec3], scanners: Set[Vec3]): (Set[Vec3], Set[Vec3]) =
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
        go(next, beacons, pack, scanners ++ positions)

    go(scanners.tail, Set.empty, scanners.head.report, Set(Vec3.origin))
  }

  val (beacons, positions) = solve(scanners) 

  val answer1 = beacons.size
  println(s"answer 1: $answer1 [${System.currentTimeMillis - start}ms]")
  assert(answer1 == 320)

  val answer2 =
    val distances = for {
      a <- positions
      b <- positions
    } yield Vec3.distance(b, a)    
    distances.max

  println(s"answer 2: $answer2 [${System.currentTimeMillis - start}ms]")
  assert(answer2 == 9655)


  

