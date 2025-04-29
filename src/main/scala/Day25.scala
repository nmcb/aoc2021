import scala.io.*

object Day25 extends App:
  val start = System.currentTimeMillis

  case class Pos(x: Int, y: Int):
    def +(that: Pos): Pos =
      Pos(x + that.x, y + that.y)

  case class Floor(tiles: Map[Pos,Char], maxX: Int, maxY: Int):
    assert(tiles.size == (maxX + 1) * (maxY + 1))

    override def toString: String =
      (0 to maxY).foldLeft("")((s,y) =>
        s + "\n" + (0 to maxX).foldLeft("")((ss,x) =>
          ss + tiles(Pos(x,y)).toString))

    def eastFacing: Set[Pos] =
      tiles.filter((p,c) => c == '>').keySet
    
    def southFacing: Set[Pos] =
      tiles.filter((p,c) => c == 'v').keySet
    
    def eastOf(p: Pos): Pos =
      if (p.x < maxX) p + Pos(1,0) else p.copy(x = 0)
    
    def southOf(p: Pos): Pos =
      if (p.y < maxY) p + Pos(0,1) else p.copy(y = 0)

    def turn: Floor = //Map[Pos,Char] =
      val (emove, eremoved) = tiles.partition((p,c) => c == '>')
      val emoved  = emove.map((p,c) => if (tiles(eastOf(p)) == '.') p -> '.' else p -> c)
      val eupdate = emove.filter((p,c) => tiles(eastOf(p)) == '.').map((p,c) => eastOf(p) -> c)
      val entiles = eremoved ++ eupdate ++ emoved

      val (smove, sremoved) = entiles.partition((p,c) => c == 'v')
      val smoved  = smove.map((p,c) => if (entiles(southOf(p)) == '.') p -> '.' else p -> c)
      val supdate = smove.filter((p,c) => entiles(southOf(p)) == '.').map((p,c) => southOf(p) -> c)
      val sntiles = sremoved ++ supdate ++ smoved
      copy(tiles = sntiles)


  val floor =
    val tiles = Source
      .fromFile("src/main/resources/input25.txt")
      .getLines
      .zipWithIndex
      .foldLeft(Map.empty[Pos,Char]){ case (a,(l,y)) =>
        l.zipWithIndex.foldLeft(a) { case (a,(c,x)) =>
          a + (Pos(x,y) -> c)
      }}
    Floor(tiles, tiles.map(_._1.x).max, tiles.map(_._1.y).max)


  def solve(f: Floor, i: Int = 1): Int =
    val nf = f.turn
    if (f == nf) i
    else solve(nf, i + 1)

  println(solve(floor))