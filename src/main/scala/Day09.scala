import scala.io._

object Day09 extends App:
  val start = System.currentTimeMillis

  def ﷽[A](a: A): A = a

  val floor =
    Floor(Source
      .fromFile("src/resources/input09.txt")
      .getLines
      .toList
      .map(line => line.toList.map(_.toString.toInt)))

  case class Floor(loc: List[List[Int]]):
    val maxX = loc.head.size
    val maxY = loc.size

    def sample(x: Int, y: Int): Option[Int] =
      if (y < 0 || y >= maxY) None
      else if (x < 0 || x >= maxX) None
      else Some(loc(y)(x))

    def height(x: Int, y: Int): Int =
      sample(x,y).getOrElse(sys.error("boom!"))

    def neighbourHeights(x: Int, y: Int): List[((Int,Int), Int)] =
      List((1,0),(-1,0),(0,1),(0,-1))
        .map((nx,ny) => ((nx,ny), sample(x + nx, y + ny)))
        .filterNot(_._2 == None)
        .map((l,h) => ((x + l._1,y + l._2),h.get))

    def upstream(x: Int, y: Int): List[(Int,Int)] =
      neighbourHeights(x,y)
        // FIX two adjecent of same height
        .filter((l,nh) => (nh < 9) && (nh > height(x,y)))
        .map(_._1)
        
  val heights =
    var result: List[((Int,Int),Int)] = List.empty // shoot me!
    for (x <- (0 until floor.maxX) ; y <- (0 until floor.maxY)) {
      val height = floor.height(x, y)
      if (floor.neighbourHeights(x, y).forall { case ((nx,ny),nh) => height < nh })
        result = ((x,y), height) :: result
      else
        ()
    }
    result

  val answer1 = heights.map((_,h) => h + 1).sum
  println(s"Answer 1 = ${answer1} [${System.currentTimeMillis - start}ms]")
  assert(answer1 == 502)

  val answer2: Int = 
    def loop(todo: List[(Int,Int)], acc: List[(Int,Int)] = List.empty): Int =
      if (todo.isEmpty)
        acc.size
      else
        val (x, y)   = todo.head
        val upstream = floor.upstream(x, y)
        loop(upstream ++ todo.tail, ((x,y) :: acc).distinct)
    
    heights
      .map((point,_) => loop(List((point._1,point._2))))
      .sorted
      .reverse
      .take(3)
      .product

  println(s"Answer 2 = ${answer2} [${System.currentTimeMillis - start}ms]")
  assert(answer2 == 1330560)
