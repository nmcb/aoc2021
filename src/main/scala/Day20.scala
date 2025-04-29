object Day20 extends App:
  val start = System.currentTimeMillis

  case class Pos(x: Int, y: Int):
    def +(that: Pos): Pos =
      Pos(x + that.x, y + that.y)

  case class Image(pixels: List[List[Boolean]], canvas: Boolean)

  val offsets =
    for { dy <- -1 to 1 ; dx <- -1 to 1 } yield Pos(dx, dy)

  def within(grid: List[List[Boolean]], pos: Pos): Boolean =
    0 <= pos.x && 0 <= pos.y && pos.y < grid.size && pos.x < grid(pos.y).size

  def peek(grid: List[List[Boolean]], pos: Pos): Boolean =
    grid(pos.y)(pos.x)

  def int(binary: Seq[Boolean]): Int =
    binary.foldLeft(0)((acc, bit) => (acc << 1) | (if bit then 1 else 0))

  def enhance(algorithm: Seq[Boolean], image: Image): Image =
    val Image(pixels, canvas) = image
    val frame    = List.fill(pixels(0).size + 2)(canvas)
    val framed   = frame +: pixels.map(canvas +: _ :+ canvas) :+ frame
    val enhanced =
      for ((row, y) <- framed.zipWithIndex) yield
        for ((_, x) <- row.zipWithIndex) yield
          val pos  = Pos(x, y)
          val area = offsets.map(_ + pos)
          val bin  = area.map(cell =>
            if (within(framed, cell)) peek(framed, cell)
            else canvas)
          algorithm(int(bin))
  
    val Int9x0 = 0
    val Int9x1 = 511
    val background = algorithm(if (canvas) Int9x1 else Int9x0)
    Image(enhanced, background)

  def parseImage(s: String): Image =
    Image(s.linesIterator.map(_.map(_ == '#').toList).toList, false)

  def parse(input: String) =
    val Seq(algorithmStr, imageStr) = input.split("\n\n").toSeq
    val algorithm = algorithmStr.map(_ == '#').toList
    val image = parseImage(imageStr)
    (algorithm, image)

  val input =
    io.Source
      .fromFile("src/main/resources/input20.txt")
      .mkString

  val (algorithm, image0) = parse(input)
  val image1  = enhance(algorithm, image0)
  val answer1 = image1.pixels.map(_.count(_ == true)).sum
  println(s"answer 1: $answer1 [${System.currentTimeMillis - start}ms]")
  assert(answer1 == 5298)

  val image50 = (1 to 50).foldLeft(image0)((img,_) => enhance(algorithm, img))
  val answer2 = image50.pixels.map(_.count(_ == true)).sum
  println(s"answer 2: $answer2 [${System.currentTimeMillis - start}ms]")
  assert(answer2 == 17548)
